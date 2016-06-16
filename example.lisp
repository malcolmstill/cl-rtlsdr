
(defpackage :rtlsdr-example
  (:use :common-lisp :rtlsdr)
  (:export :run-example))

(in-package :rtlsdr-example)

(defun run-example ()
  (let* ((sample-rate 2048000)
	(n-samples (* 4000 512))
	(buffer-length n-samples))
    (with-rtlsdr (device 0)
      (set-center-frequency device (MHz 100))
      (set-sample-rate device sample-rate)
      (set-tuner-gain-mode device 0)
      (reset-buffer device)
      (time
       (aref (read-sync device buffer-length) 0)))))
