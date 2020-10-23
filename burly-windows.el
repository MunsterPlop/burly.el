;;; burly-windows.el --- Burly window configuration library  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Requirements


;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(defun burly-windows--serialize-window-configuration (&optional window-data-fn)
  (list :frame (list :width (frame-width)
                     :height (frame-height))
        :windows (cl-loop for window in (window-list)
                          collect (list :edges (window-edges window)
                                        :selected-p (eq (selected-window) window)
                                        :data (when window-data-fn
                                                (funcall window-data-fn window))))))

(defun burly-windows--restore-window-configuration (config)
  (delete-other-windows)
  (pcase-let* (((map (:frame (map :width :height))
                     :windows)
                config)
               ;; Normalize edges.
               (frame-width-factor (round (/ (float (frame-width)) (float width))))
               (frame-height-factor (round (/ (float (frame-height)) (float height))))
               (windows (cl-loop for window in windows
                                 ;; Avoid modifying the CONFIG passed to this function.
                                 do (setf window (copy-seq window)
                                          ;; Normalize window edges in case frame has been resized.
                                          (map-elt window :edges)
                                          (pcase-let (((map (:edges `(,left ,top ,right ,bottom))) window))
                                            (list (* left frame-width-factor)
                                                  (* top frame-height-factor)
                                                  (* right frame-width-factor)
                                                  (* bottom frame-height-factor))))
                                 collect window))
               (first-window (cl-loop for window in windows
                                      when (pcase window
                                             ((map (:edges `(0 0 ,_ ,_))) t))
                                      return window))
               (other-windows (cl-delete first-window windows :test #'equal)))
    (cl-assert first-window)
    ;; Go through the list of saved windows, splitting when an
    ;; appropriate, existing window is found.  It may be necessary to
    ;; go through the list more than once before all config are
    ;; recreated.
    (cl-loop for i below 100
             do (when (eq i 99)
                  (error "Infinite loop detected"))
             until (zerop (length other-windows))
             do (cl-loop for saved-window in other-windows
                         do (pcase-let* (((map (:edges `(,saved-left ,saved-top ,_right ,_bottom))) saved-window))
                              ;; Find window with same left edge.
                              (if-let* ((same-left-edge-window (cl-loop for window in (window-list)
                                                                        for (left top _ _) = (window-edges window)
                                                                        when (eq left saved-left)
                                                                        return window)))
                                  ;; Found window with same left edge: split vertically (or...horizontally?).
                                  (with-selected-window same-left-edge-window
                                    (split-window-horizontally)
                                    (setf other-windows (cl-delete saved-window other-windows :test #'equal)))
                                ;; Find window with same top edge.
                                (if-let* ((same-top-edge-window (cl-loop for window in (window-list)
                                                                         for (left top _ _) = (window-edges window)
                                                                         when (eq top saved-top)
                                                                         return window)))
                                    ;; Found window with same top edge: split horizontally (or...vertically?).
                                    (with-selected-window same-top-edge-window
                                      (split-window-vertically)
                                      (setf other-windows (cl-delete saved-window other-windows :test #'equal))))))))
    ;; TODO: Reselect selected window.
    ;; TODO: Restore buffers.
    ))

(defun burly-windows--restore-window-configuration (config)
  (cl-labels ((window-sharing
               ;; Return the window that shares NUM EDGES.
               (num edges) (cl-loop for window in (window-list)
                                    for existing-edges = (window-edges window)
                                    for num-matching = (cl-loop for i from 0 to 3
                                                                count (eq (nth i edges) (nth i existing-edges)))
                                    when (eq num num-matching)
                                    return window))
              (split (window new-edges)
                     ;; Split WINDOW according to which of NEW-EDGES its edges match.
                     (pcase-let* ((`(,new-left ,new-top ,new-right ,new-bottom) new-edges)
                                  (`(,left ,top ,right ,bottom) (window-edges window))
                                  (side (cond ((not (eq new-left left)) 'right)
                                              ((not (eq new-top top)) 'below)
                                              ((not (eq new-right right)) 'left)
                                              ((not (eq new-bottom bottom)) 'above)))
                                  ;; TODO: Calculate size of new window.
                                  )
                       (split-window window nil side))))
    (delete-other-windows)
    (pcase-let* (((map (:frame (map :width :height))
                       :windows)
                  config)
                 (frame-width-factor (round (/ (float (frame-width)) (float width))))
                 (frame-height-factor (round (/ (float (frame-height)) (float height))))
                 (windows (cl-loop for window in windows
                                   ;; Avoid modifying the CONFIG passed to this function.
                                   do (setf window (copy-seq window)
                                            ;; Normalize window edges in case frame has been resized.
                                            (map-elt window :edges)
                                            (pcase-let (((map (:edges `(,left ,top ,right ,bottom))) window))
                                              (list (* left frame-width-factor)
                                                    (* top frame-height-factor)
                                                    (* right frame-width-factor)
                                                    (* bottom frame-height-factor))))
                                   collect window))
                 (first-window (cl-loop for window in windows
                                        when (pcase window
                                               ((map (:edges `(0 0 ,_ ,_))) t))
                                        return window))
                 (other-windows (cl-delete first-window windows :test #'equal))
                 (restore-frame-width (frame-width))
                 (restore-frame-height (frame-height)))
      (cl-assert first-window)
      ;; Go through the list of saved windows, splitting when an appropriate, existing window is found.
      ;; It may be necessary to go through the list more than once before all windows are recreated.
      (cl-loop for i below 100
               do (when (eq i 99)
                    (error "Infinite loop detected"))
               until (zerop (length other-windows))
               do (cl-loop for saved-window in other-windows
                           do (pcase-let* (((map :edges) saved-window)
                                           (parent-window (window-sharing 2 edges)))
                                (when parent-window
                                  (split parent-window edges)
                                  (setf other-windows (cl-delete saved-window other-windows :test #'equal))))))
      ;; TODO: Reselect selected window.
      ;; TODO: Restore buffers.
      )))

;;;; Footer

(provide 'burly-windows)

;;; burly-windows.el ends here
