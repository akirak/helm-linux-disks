;;; helm-linux-disks.el --- Helm interface for managing removable volumes in Linux -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (helm "1.9.4"))
;; URL: https://github.com/akirak/helm-linux-disks

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This function provides `helm-linux-disks' command which is a convenient
;; interface for running operations on (removable) volumes in Linux.

;;; Code:

(require 'linux-disk)
(require 'helm)
(require 'cl-lib)
(require 'seq)

;;;###autoload
(defun helm-linux-disks ()
  "Display a list of block devices via Helm."
  (interactive)
  (helm :prompt "block device: "
        :buffer "*helm lsblk*"
        :sources '(helm-linux-disks--lsblk-source
                   helm-linux-disks--vgs-source)))

(defvar helm-linux-disks--lsblk-source
  (helm-build-sync-source "lsblk (-o name,mountpoint,fstype,type,size)"
    :action '(("dwim (mount/unmount)" . linux-disk-dwim)
              ("mount (udisksctl)" . linux-disk-udisksctl-mount)
              ;; TODO
              ;; ("mount (mount)" . linux-disk-builtin-mount)
              ("unmount (udisksctl)" . linux-disk-udisksctl-unmount)
              ;; TODO
              ;; ("unmount (umount)" . linux-disk-builtin-umount)
              ("lock / close the LUKS-crypted device (udisksctl)"
               . linux-disk-udisksctl-lock)
              ("lock (cryptsetup luksClose)"
               . linux-disk-cryptsetup-luks-close)
              ("unlock / open the LUKS-crypted device (udisksctl)"
               . linux-disk-udisksctl-unlock)
              ("dired on the mount point"
               . linux-disk-dired)
              ("dired on the mount point in other window"
               . linux-disk-dired-other-window)
              ("terminal on the mount point"
               . linux-disk-terminal)
              ("helm-find-files-1 from the mount point"
               . (lambda (struct)
                   (interactive)
                   (let ((mountpoint (linux-disk-mountpoint struct)))
                     (unless mountpoint
                       (error "Not mounted"))
                     (helm-find-files-1 (file-name-as-directory mountpoint)))))
              ("info (udisksctl)" . linux-disk-udisksctl-info)
              ("power off the device" . linux-disk-udisksctl-poweroff))
    :candidates 'helm-linux-disks--lsblk)
  "The primary Helm source for `helm-linux-disks'.")

(defun helm-linux-disks--lsblk ()
  "Get an alist of candidates for `helm-linux-disks--lsblk-source'."
  (cl-loop for ((level . raw) . kdr) on (helm-linux-disks--lsblk-with-levels)
           for next-level = (when kdr (caar kdr))
           for has-child = (and next-level
                                (< level next-level))
           for (name mountpoint fstype type uuid) = (pcase (split-string
                                                            (helm-linux-disks--lsblk-trim raw))
                                                      (`(,name ,type ,size)
                                                       (list name nil nil type nil size))
                                                      (`(,name ,fstype ,type ,size)
                                                       (list name nil fstype type size))
                                                      (fields fields))
           collect (cons raw
                         (make-linux-disk
                          :path name
                          :mountpoint mountpoint
                          :fstype fstype
                          :type (intern type)
                          :has-child-p has-child))))

(defun helm-linux-disks--lsblk-trim (raw)
  "Trim control characters and white spaces from an output of lsblk command.

RAW is a line in the output of lsblk command."
  (if (string-match "\\(/.+\\)$" raw)
      (match-string 1 raw)
    raw))

(defun helm-linux-disks--lsblk-with-levels ()
  "Run lsblk command and annotate each line with its level."
  (mapcar (lambda (raw-output)
            (cons (helm-linux-disks--lsblk-get-level raw-output) raw-output))
          (process-lines "sudo" "lsblk" "-n" "-p"
                         "-o" "name,mountpoint,fstype,type,size")))

(defun helm-linux-disks--lsblk-get-level (output)
  "Get the level of a record from an OUTPUT of lsblk command.

A level is the starting position of the first slash character."
  (seq-position output ?/))

(defvar helm-linux-disks--vgs-source
  (helm-build-sync-source "LVM volume groups"
    :action '(("deactivate (vgchange -an)" . helm-linux-disks--vg-deactivate)
              ("activate (vgchange -ay)" . helm-linux-disks--vg-activate)
              ("vgdisplay" . helm-linux-disks--vg-display))
    :candidates (lambda ()
                  (when (executable-find "vgs")
                    (cl-loop for s in (process-lines "sudo" "vgs"
                                                     "--noheadings")
                             collect (cons s (car (split-string s)))))))
  "Helm source containing a list of LVM volume groups for `helm-linux-disks'.

This is useful for removing a device with a LVM physical volume.")

(defun helm-linux-disks--vg-deactivate (vg)
  "Deactivate a LVM volume group named VG."
  (when (yes-or-no-p (format "Deactivate %s VG with the following LVs?\n%s"
                             vg (shell-command-to-string
                                 (format "sudo lvs %s" (shell-quote-argument vg)))))
    (call-process "sudo" nil "*vgchange*" nil "vgchange" "-an" vg)))

(defun helm-linux-disks--vg-activate (vg)
  "Activate a LVM volume group named VG."
  (call-process "sudo" nil "*vgchange*" nil "vgchange" "-ay" vg))

(defun helm-linux-disks--vg-display (vg)
  "Run vgdisplay command on a LVM volume group named VG."
  (let ((buf (generate-new-buffer-name (format "*vgdisplay %s*" vg))))
    (call-process "sudo" nil buf nil "vgdisplay" vg)
    (with-current-buffer buf
      (local-set-key (kbd "q") #'quit-window)
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;; helm-linux-disks.el ends here
(provide 'helm-linux-disks)
