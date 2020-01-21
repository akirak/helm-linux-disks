;;; remount.el --- Mount removable media -*- lexical-binding: t -*-

(defun remount--read-process (command &rest args)
  "Run COMMAND with ARGS and read its output."
  (let ((process-environment '("TERM=dumb")))
    (with-temp-buffer
      (pcase (apply #'call-process command nil t nil args)
        (0 (buffer-substring-no-properties (point-min) (point-max)))
        (n (error "Command %s exited with %d" command n))))))

(defun remount--read-json-process (command &rest args)
  "Run COMMAND with ARGS and read its output."
  (let ((json-object-type 'plist))
    (json-read-from-string (apply #'remount--read-process command args))))

(cl-defstruct remount-block-device name path size readonly mountpoint model
              fstype
              ;; disk, part, crypt, or lvm
              type
              ;; List of paths of inner devices
              children
              ;; Nested level: used for presentation
              level)

(defun remount--get-linux-block-devices ()
  (let (result)
    (cl-labels ((parse (level entry)
                       (-let (((&plist :name :path :fstype :size :ro :type :mountpoint :model :children) entry))
                         (push (make-remount-block-device
                                :name name
                                :path path
                                :fstype fstype
                                :size size
                                :readonly (eq ro :json-true)
                                :type type
                                :mountpoint mountpoint
                                :model model
                                :children
                                (seq-map (lambda (child) (plist-get child :path)) children)
                                :level level)
                               result)
                         (seq-do (-partial #'parse (1+ level)) children))))
      (seq-do (-partial #'parse 0)
              (plist-get (remount--read-json-process "lsblk" "--json" "-o" "+path,fstype,model")
                         :blockdevices)))
    (nreverse result)))

(defun remount-get-block-devices ()
  (remount--get-linux-block-devices))

(defun remount--format-device (device)
  (propertize (string-join (list (make-string (* 2 (remount-block-device-level device)) ?\s)
                                 (remount-block-device-path device))
                           "  ")
              'remount-block-device device))

(defun remount-ivy ()
  (interactive)
  (let ((devices (remount-get-block-devices)))
    (ivy-read "Block devices: "
              (mapcar #'remount--format-device devices))))

(provide 'remount)
;;; remount.el ends here
