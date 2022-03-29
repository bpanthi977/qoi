;;;; qoi.lisp

(in-package #:qoi)

(defun read-uint32-be (stream)
  "Read unsigned 32 bit integer in big endian from stream"
  (loop repeat 4
        for byte = (read-byte stream)
        for val = byte then (+ (* val #.(expt 2 8)) byte)
        finally (return val)))

(defun write-uint32-be (stream int)
  "Write unsigned 32 bit integer in big endian to stream"
  (loop for i from 3 downto 0
        for byte = (ldb (byte 8 (* i 8)) int)
        do (write-byte byte stream)))

(defmacro setf-array ((last? position array) &rest values)
  "stores `values' at consecutive position in `array'. if last? is false don't store last value"
  (u:once-only (position array)
    `(progn
       ,@(loop for v in values
               for i from 0 below (1- (length values))
               collect `(setf (aref ,array (+ ,position ,i)) ,v))
       (when ,last?
         (setf (aref ,array (+ ,position ,(1- (length values)))) ,(car (last values)))))))

(defconstant +qoi-magic+
  (reduce (lambda (a b) (+ (* a (expt 2 8)) b))
          (map 'vector #'char-code "qoif"))
  "Magic bytes as integer")

(defconstant +qoi-op-run+ #b11000000)
(defconstant +qoi-op-diff+ #b01000000)
(defconstant +qoi-op-luma+ #b10000000)
(defconstant +qoi-op-rgba+ #b11111111)
(defconstant +qoi-op-rgb+ #b11111110)

(defun decode (stream)
  "decode qoi from `stream' of (unsigned-byte 8)"
  (let ((header (read-uint32-be stream))
        (width (read-uint32-be stream))
        (height (read-uint32-be stream))
        (channels (read-byte stream))
        (colorspace (read-byte stream)))

    (unless (= header +qoi-magic+)
      (error "Magic Bytes doesn't correspond to `qoif' ~x:" header))
    (unless (<= 3 channels 4)
      (error "only RGB or RGBA channels allowed"))

    (let* ((r 0) (g 0) (b 0) (a 255)
           (alphap (= channels 4))
           (size (* width height channels))
           (image (make-array size :element-type '(unsigned-byte 8)))
           (indexarray (make-array (* 64 channels)
                                   :element-type '(unsigned-byte 8)))
           (i 0))

      (flet ((push-pixel (rr gg bb aa)
               (setf r rr g gg b bb a aa)
               (let ((hash (mod (+ (* r 3) (* g 5) (* b 7) (* a 11)) 64)))
                 (setf-array (alphap (* hash channels) indexarray) r g b a)
                 (setf-array (alphap i image) r g b a))
               (incf i channels)))

        (loop while (not (= i size))
              for byte = (read-byte stream)  do
                (cond
                  ;; QOI_OP_RGB
                  ((= byte +qoi-op-rgb+)
                   (push-pixel (read-byte stream) (read-byte stream) (read-byte stream) a))
                  ;; QOI_OP_RGBA
                  ((= byte +qoi-op-rgba+)
                   (push-pixel (read-byte stream) (read-byte stream) (read-byte stream) (read-byte stream)))
                  (t
                   (case (ldb (byte 2 6) byte)
                     ;; QOI_OP_INDEX
                     (#b00
                      (let ((index (* (ldb (byte 6 0) byte) channels)))
                        (push-pixel (aref indexarray (+ index 0))
                                    (aref indexarray (+ index 1))
                                    (aref indexarray (+ index 2))
                                    (if alphap (aref indexarray (+ index 3)) a))))

                     ;; QOI_OP_DIFF
                     (#b01
                      (push-pixel (mod (+ r (ldb (byte 2 4) byte) -2) 256)
                                  (mod (+ g (ldb (byte 2 2) byte) -2) 256)
                                  (mod (+ b (ldb (byte 2 0) byte) -2) 256)
                                  a))

                     ;; QOI_OP_LUMA
                     (#b10
                      (let* ((dg  (+ (ldb (byte 6 0) byte) -32))
                             (byte2 (read-byte stream))
                             (dr-dg (+ (ldb (byte 4 4) byte2) -8))
                             (db-dg (+ (ldb (byte 4 0) byte2) -8)))
                        (push-pixel (mod (+ r dg dr-dg) 256)
                                    (mod (+ g dg) 256)
                                    (mod (+ b dg db-dg) 256)
                                    a)))

                     ;; QOI_OP_RUN
                     (#b11
                      (let ((run (1+ (ldb (byte 6 0) byte))))
                        (loop repeat run do
                          (setf-array (alphap i image) r g b a)
                          (incf i channels)))))))))
      (values image width height channels colorspace))))

(defmacro copy-array ((from m) (to n) alphap)
  (u:once-only (from m to n)
    `(progn
       ,@(loop for i from 0 to 2
               collect `(setf (aref ,to (+ ,n ,i)) (aref ,from (+ ,m ,i))))
       (when ,alphap
         (setf (aref ,to (+ ,n 3)) (aref ,from (+ ,m 3)))))))

(defun encode (stream image width height channels colorspace)
  "encode `image' in `qoi' format; writing to `stream' of type (unsigned-byte 8)"
  ;; Header
  (loop for char across "qoif"
        do (write-byte (char-code char) stream))
  (write-uint32-be stream width)
  (write-uint32-be stream height)
  (assert (<= 3 channels 4))
  (write-byte channels stream)
  (assert (<= 0 colorspace 1))
  (write-byte colorspace stream)

  ;; data
  (let ((alphap (= channels 4))
        (pixels-count (* width height))
        (indexarray (make-array (* 64 4) :element-type '(unsigned-byte 32)))
        (prev-pixel (make-array 4 :element-type '(unsigned-byte 8)))
        (pixel (make-array 4 :element-type '(unsigned-byte 8)))
        (i 0))
    ;; first pixel is 0 0 0 255
    (setf (aref pixel 3) 255)

    (labels ((read-pixel ()
               (copy-array (pixel 0) (prev-pixel 0) t)
               (copy-array (image (* i channels)) (pixel 0) alphap)
               (incf i)
               pixel)

             (index-pixel ()
               (let ((hash (mod (+ (* (aref pixel 0) 3)
                                   (* (aref pixel 1) 5)
                                   (* (aref pixel 2) 7)
                                   (* (if alphap
                                          (aref pixel 3)
                                          255)
                                      11))
                                64)))
                 (copy-array (pixel 0) (indexarray (* hash 4)) t)))

             (position-in-index (pixel)
               ;; position of pixel in index
               (loop for i from 0 to (* 63 4) by 4
                     for pos from 0
                     when (and (eql (aref indexarray (+ i 0)) (aref pixel 0))
                               (eql (aref indexarray (+ i 1)) (aref pixel 1))
                               (eql (aref indexarray (+ i 2)) (aref pixel 2))
                               (eql (aref indexarray (+ i 3)) (aref pixel 3)))
                       do (return pos)))

             (op-run ()
               (when (every #'eql pixel prev-pixel)
                 (let ((run 1))
                   (loop repeat (min 61 (- pixels-count i))
                         while (every #'eql (read-pixel) prev-pixel)
                         do (incf run)
                         finally (write-byte (+ +qoi-op-run+ (1- run)) stream))
                   run)))

             (op-diff ()
               (let ((dr (- (aref pixel 0) (aref prev-pixel 0)))
                     (dg (- (aref pixel 1) (aref prev-pixel 1)))
                     (db (- (aref pixel 2) (aref prev-pixel 2))))
                 (when (and (<= -2 dr 1)
                            (<= -2 dg 1)
                            (<= -2 db 1))
                   (write-byte (dpb  (+ dr 2) (byte 2 4)
                                     (dpb (+ dg 2) (byte 2 2)
                                          (dpb (+ db 2) (byte 2 0)
                                               +qoi-op-diff+)))
                               stream))))

             (op-index ()
               (let ((index (position-in-index pixel)))
                 (when index
                   (write-byte index stream))))

             (op-luma ()
               (let* ((dg (- (aref pixel 1) (aref prev-pixel 1)))
                      (dr-dg (- (aref pixel 0) (aref prev-pixel 0) dg))
                      (db-dg (- (aref pixel 2) (aref prev-pixel 2) dg)))
                 (when (and (<= -32 dg 31)
                            (<= -8 dr-dg 7)
                            (<= -8 db-dg 7))
                   (write-byte (+ +qoi-op-luma+ (+ dg 32)) stream)
                   (write-byte (dpb (+ dr-dg 8) (byte 4 4) (+ db-dg 8))
                               stream)))))

      (loop named outer-loop
            while (not (= i pixels-count)) do
         (read-pixel)
         ;; first try as many runs as possible
         (loop for run = (op-run)
               while (and run (= run 62))
               do
                  (if (= i pixels-count)
                    (return-from outer-loop)
                    (read-pixel)))
         ;; try op-diff then op-index then op-luma and finally rgb(a)
         (let ((same-alpha? (= (aref pixel 3) (aref prev-pixel 3))))
           (or (and same-alpha? (op-diff))
               (op-index)
               (and same-alpha? (op-luma))
               (progn
                 ;; QOI_OP_RGB(A)
                 (if alphap
                     (write-byte +qoi-op-rgba+ stream)
                     (write-byte +qoi-op-rgb+ stream))
                 (loop for i from 0 below channels
                       do (write-byte (aref pixel i) stream))))
           (index-pixel)))))
  stream)
