;; Common Lisp bindings to librtlsdr for great justice!
;; Copyright Malcolm Still <malcolm.still@gmail.com>

;; 1.0 House keeping

(asdf:oos 'asdf:load-op :cffi)

(defpackage :rtlsdr
  (:use :common-lisp :cffi)
  (:export :MHz
	   :get-device-count
	   :get-device-name
	   :get-device-usb-strings
	   :get-index-by-serial
	   :open-device
	   :close-device
	   :with-rtlsdr
	   :set-crystal-frequency
	   :get-crystal-frequency
	   :get-usb-strings
	   :write-eeprom
	   :read-eeprom
	   :set-center-frequency
	   :get-center-frequency
	   :set-frequency-correction
	   :get-frequency-correction
	   :get-tuner-type
	   :get-tuner-gains
	   :set-tuner-gain
	   :get-tuner-gain
	   :set-tuner-if-gain
	   :set-tuner-gain-mode
	   :set-sample-rate
	   :get-sample-rate
	   :set-testmode
	   :set-agc-mode
	   :set-direct-sampling
	   :get-direct-sampling
	   :set-offset-tuning
	   :get-offset-tuning
	   :reset-buffer
	   :read-sync
	   :read-async
	   :cancel-read-async
	   :define-read-async-callback))

(in-package :rtlsdr)

(define-foreign-library librtlsdr
    (t (:default "librtlsdr")))

(use-foreign-library librtlsdr)

(defun MHz (m)
  (* 1000 1000 m))

(defun multiple-of-512 (x)
  (= (mod x 512) 0))

;; Buffer length used with read-sync (and read-async?) must be multiple
;; of 512
(deftype rtlsdr-buffer ()
  `(satisfies multiple-of-512))

;; 1.1 Talking to the device(s)

; Get device count
(defcfun ("rtlsdr_get_device_count" get-device-count) :uint32)

; How to handle const char* return with CFFI?
(defcfun ("rtlsdr_get_device_name" get-device-name) :pointer
  (index :uint32))

; Need to allocate memory with CFFI and then pass to librtlsdr to be populated
(defcfun "rtlsdr_get_device_usb_strings" :int32
  (index :uint32)
  (manufact :pointer)
  (product :pointer)
  (serial :pointer))

(defun get-device-usb-strings (index)
  (let ((manufact (foreign-alloc :char :count 256))
	(product (foreign-alloc :char :count 256))
	(serial (foreign-alloc :char :count 256)))
    (rtlsdr-get-device-usb-strings index manufact product serial)
    (let ((m (foreign-string-to-lisp manufact))
	  (p (foreign-string-to-lisp product))
	  (s (foreign-string-to-lisp serial)))
      (foreign-free manufact)
      (foreign-free product)
      (foreign-free serial)
      (values m p s))))

; This appears to work. Returns -3 on fail otherwise the index.
(defcfun ("rtlsdr_get_index_by_serial" get-index-by-serial) :int32
  (serial :string))

; Think this seems to work by passing (foreign-alloc :pointer) as dev
(defcfun ("rtlsdr_open" open-device) :int32
  (ptr->device-ptr :pointer)
  (index :uint32))

; This should be trivial once open-device is done
(defcfun ("rtlsdr_close" close-device) :int32
  (dev :pointer))

;; I was hoping the unwind-protect would stop us having to reconnect the RTLSDR
;; if error occurs within body.
(defmacro with-rtlsdr ((device index) &body body)
  (let ((ptr->device-ptr (gensym "**dev")))
    `(let ((,ptr->device-ptr (foreign-alloc :pointer)))
       (if (< (open-device ,ptr->device-ptr ,index) 0)
	   (error "Failed to open device at index ~A." ,index)
	   (let* ((,device (mem-ref ,ptr->device-ptr :pointer)))
	     (unwind-protect
		  (progn ,@body)
	       (close-device ,device)
	       (foreign-free ,ptr->device-ptr)))))))

;; 1.2 Configuration functions

;; Set crystal oscillator frequencies
(defcfun ("rtlsdr_set_xtal_freq" set-crystal-frequency) :int32
  (dev :pointer)
  (rtl-frequency :uint32)
  (tuner-frequency :uint32))

;; Get crystal oscillator frequencies
(defcfun ("rtlsdr_get_xtal_freq" get-crystal-frequency) :int32
  (dev :pointer)
  (rtl-frequency :uint32)
  (tuner-frequency :uint32))

; Need to allocate memory with CFFI and then pass to librtlsdr to be populated
(defcfun "rtlsdr_get_usb_strings" :int32
  (dev :pointer)
  (manufacturer :pointer)
  (product :pointer)
  (serial :pointer))

(defun get-usb-strings (device)
  (let ((manufacturer (foreign-alloc :char :count 256))
	(product (foreign-alloc :char :count 256))
	(serial (foreign-alloc :char :count 256)))
    (rtlsdr-get-usb-strings device manufacturer product serial)
    (let ((m (foreign-string-to-lisp manufacturer))
	  (p (foreign-string-to-lisp product))
	  (s (foreign-string-to-lisp serial)))
      (foreign-free manufacturer)
      (foreign-free product)
      (foreign-free serial)
      (values m p s))))

; Write EEPROM
(defcfun ("rtlsdr_write_eeprom" write-eeprom) :int32
  (device :pointer)
  (data :pointer)
  (offset :uint8)
  (length :uint16))

; Read EEPROM
(defcfun ("rtlsdr_read_eeprom" read-eeprom) :int32
  (device :pointer)
  (data :pointer)
  (offset :uint8)
  (length :uint16))

; Set center frequency. frequency in Hz.
(defcfun ("rtlsdr_set_center_freq" set-center-frequency) :int32
  (device :pointer)
  (frequency :uint32))

; Get center frequency. Return value in Hz.
(defcfun ("rtlsdr_get_center_freq" get-center-frequency) :int32
  (device :pointer))

; Set frequency correction
(defcfun ("rtlsdr_set_freq_correction" set-frequency-correction) :int32
  (device :pointer)
  (ppm :int32))

; Get frequency correction
(defcfun ("rtlsdr_get_freq_correction" get-frequency-correction) :int32
  (device :pointer))

; Get tuner type
(defcenum tuner
  :unknown
  :e4000
  :fc0012
  :fc0013
  :fc2580
  :r820t
  :r828d)

(defcfun ("rtlsdr_get_tuner_type" get-tuner-type) tuner
  (device :pointer))

; Get list of tuner gains
(defcfun ("rtlsdr_get_tuner_gains" get-tuner-gains) :int32
  (device :pointer)
  (gains :pointer))

; Set tuner gains
(defcfun ("rtlsdr_set_tuner_gain" set-tuner-gain) :int32
  (device :pointer)
  (gain :int32))

; Get tuner gain device is configure to
(defcfun ("rtlsdr_get_tuner_gain" get-tuner-gain) :int32
  (device :pointer))

; Set tuner IF gain
(defcfun ("rtlsdr_set_tuner_if_gain" set-tuner-if-gain) :int32
  (device :pointer)
  (stage :int32)
  (gain :int32))

; Set tuner gain mode
(defcfun ("rtlsdr_set_tuner_gain_mode" set-tuner-gain-mode) :int32
  (device :pointer)
  (manual :int32))

; Set sample rate
(defcfun ("rtlsdr_set_sample_rate" set-sample-rate) :int32
  (device :pointer)
  (rate :uint32))

; Get sample rate
(defcfun ("rtlsdr_get_sample_rate" get-sample-rate) :int32
  (device :pointer))

; Enable test mode
(defcfun ("rtlsdr_set_testmode" set-testmode) :int32
  (device :pointer)
  (on :int32))

; Enable/disable the internal digital AGC of the RTL2832
(defcfun ("rtlsdr_set_agc_mode" set-agc-mode) :int32
  (device :pointer)
  (on :int32))

; Enable/disable direct sampling mode
(defcfun ("rtlsdr_set_direct_sampling" set-direct-sampling) :int32
  (device :pointer)
  (on :int32))

; Get direct sampling mode
(defcfun ("rtlsdr_get_direct_sampling" get-direct-sampling) :int32
  (device :pointer))

; Set offset tuning
(defcfun ("rtlsdr_set_offset_tuning" set-offset-tuning) :int32
  (device :pointer)
  (on :int32))

; Get offset tuning
(defcfun ("rtlsdr_get_offset_tuning" get-offset-tuning) :int32
  (device :pointer))

;; 1.3 Streaming functions

; Reset buffer
(defcfun ("rtlsdr_reset_buffer" reset-buffer) :int32
  (device :pointer))

; Read sync
(defcfun ("rtlsdr_read_sync" rtlsdr-read-sync) :int32
  (device :pointer)
  (buffer :pointer)
  (length :int32)
  (n-read :pointer))

(defun read-sync (device buffer-length)
  (declare (type rtlsdr-buffer buffer-length))
  (with-foreign-objects ((buffer :uint8 buffer-length) (n-read :uint32))
    (rtlsdr-read-sync device buffer buffer-length n-read)
    (let ((samples (make-array (mem-aref n-read :uint32))))
      (loop :for i :from 0 :to (- (mem-aref n-read :uint32) 1)
	 :do (setf (aref samples i) (mem-aref buffer :uint8)))
      samples)))

; Read async
(defcfun ("rtlsdr_read_async" read-async) :int32
  (device :pointer)
  (callback :pointer)
  (context :pointer)
  (buffer-count :uint32)
  (buffer-length :uint32))

; Cancel async
(defcfun ("rtlsdr_cancel_async" cancel-async) :int32
  (device :pointer))

; 1.4 Utilities

(defmacro define-read-async-callback (name (buffer length context) &body body)
  `(progn (defcallback ,name :void ((,buffer :pointer) 
				    (,length :uint32) 
				    (,context :pointer))
	    ,@body)))

;; PURE BIG MAD BOAT MAN
