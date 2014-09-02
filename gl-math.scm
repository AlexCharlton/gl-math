(module gl-math *

(import chicken scheme foreign srfi-1 extras)
(import-for-syntax matchable data-structures)
(use lolevel srfi-4)

(foreign-declare "#include <hypermath.h>")

;;; Angle operations
(define degrees->radians
  (foreign-lambda float "hpmDegreesToRadians" float))

(define radians->degrees
  (foreign-lambda float "hpmRadiansToDegrees" float))

(define pi 3.14159265358979)
(define pi/2 (/ pi 2))

;;; Vector operations
(define (make-point x y z #!optional non-gc?)
  (let ((v (make-f32vector 3 0 non-gc?)))
    (f32vector-set! v 0 x)
    (f32vector-set! v 1 y)
    (f32vector-set! v 2 z)
    v))

(define (point-x p)
  (f32vector-ref p 0))

(define (point-y p)
  (f32vector-ref p 1))

(define (point-z p)
  (f32vector-ref p 2))

(define (point-x-set! p v)
  (f32vector-set! p 0 v))

(define (point-y-set! p v)
  (f32vector-set! p 1 v))

(define (point-z-set! p v)
  (f32vector-set! p 2 v))

(define (m*vector! matrix vector)
  (cond
   ((pointer? matrix)
    ((foreign-lambda void "hpmMat4VecMult" c-pointer f32vector) matrix vector))
   ((f32vector? matrix)
    ((foreign-lambda void "hpmMat4VecMult" f32vector f32vector) matrix vector))
   (else (error 'm*vector! "Wrong argument type" matrix)))
  vector)

(define (m*vector-array! matrix vector #!key (stride 0) (length 0))
  (when (and (< stride 3) (not (zero? stride)))
    (error 'm*vector-array! "Stride must be at least 3" stride))
  (cond
   ((f32vector? vector)
    ((cond
      ((f32vector? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" f32vector f32vector size_t size_t))
      ((pointer? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" c-pointer f32vector size_t size_t))
      (else (error 'm*vector-array! "Wrong argument type" matrix)))
     matrix vector (quotient (f32vector-length vector)
                             (if (zero? stride) 3 stride))
     (* stride 4)))
   ((pointer? vector)
    (when (< length 1)
      (error 'm*vector-array! "length must be given (and positive) when vector is a pointer" length))
    ((cond
      ((f32vector? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" f32vector c-pointer size_t size_t))
      ((pointer? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" c-pointer c-pointer size_t size_t))
      (else (error 'm*vector-array! "Wrong argument type" matrix)))
     matrix vector length (* stride 4)))
   (else (error 'm*vector-array! "Wrong argument type" vector)))
  vector)

(define (get-result r)
  (cond
   ((f32vector? r) r)
   ((boolean? r) (make-f32vector 3 0 r))
   (else (error 'vector-operation "Wrong argument type" r))))

(define (cross-product a b #!optional r)
  (let ((res (get-result r)))
    ((foreign-lambda void "hpmCross"
       f32vector f32vector f32vector)
     a b res)
    res))

(define (v+ a b #!optional r)
  (let ((res (get-result r)))
    ((foreign-lambda void "hpmAddVec"
       f32vector f32vector f32vector)
     a b res)
    res))

(define (v- a b #!optional r)
  (let ((res (get-result r)))
    ((foreign-lambda void "hpmSubVec"
       f32vector f32vector f32vector)
     a b res)
    res))

(define (v* v m #!optional r)
  (let ((res (get-result r)))
    ((foreign-lambda void "hpmMultVec"
       f32vector float f32vector)
     v m res)
    res))

(define vector-magnitude
  (foreign-lambda float "hpmMagnitude"
    f32vector))

(define (normalize! v)
  ((foreign-lambda void "hpmNormalize" f32vector) v)
  v)

(define dot-product
  (foreign-lambda float "hpmDot"
                  f32vector f32vector))

(define (lerp a b t #!optional r)
  (let ((res (get-result r)))
    ((foreign-lambda void "hpmLerp"
       f32vector f32vector float f32vector)
     a b t res)
    res))

;;; Quaternion operations
(define (make-quaternion x y z w #!optional non-gc?)
  (let ((v (make-f32vector 4 0 non-gc?)))
    (f32vector-set! v 0 x)
    (f32vector-set! v 1 y)
    (f32vector-set! v 2 z)
    (f32vector-set! v 3 2)
    v))

(define (quaternion-x q)
  (f32vector-ref q 0))

(define (quaternion-y q)
  (f32vector-ref q 1))

(define (quaternion-z q)
  (f32vector-ref q 2))

(define (quaternion-w q)
  (f32vector-ref q 3))

(define (quaternion-x-set! q v)
  (f32vector-set! q 0 v))

(define (quaternion-y-set! q v)
  (f32vector-set! q 1 v))

(define (quaternion-z-set! q v)
  (f32vector-set! q 2 v))

(define (quaternion-w-set! q v)
  (f32vector-set! q 3 v))

(define quaternion-normalize!
  (foreign-lambda void "hpmQuatNormalize" f32vector))

(define (get-q-result r)
  (cond
   ((f32vector? r) r)
   ((boolean? r) (make-f32vector 4 0 r))
   (else (error 'vector-operation "Wrong argument type" r))))

(define (quaternion-inverse q #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmQuatInverse" f32vector f32vector)
     q r)
    res))

(define (quaternion-cross-product a b #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmCross"
       f32vector f32vector f32vector)
     a b res)
    res))

(define (quaternion-rotate-point! q p)
  ((foreign-lambda void "hpmQuatVecRotate"
     f32vector f32vector)
   q p)
  p)

(define (quaternion-axis-angle-rotation axis angle #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmAxisAngleQuatRotation"
       f32vector float f32vector)
     axis angle res)
    res))

(define (quaternion-rotate-axis-angle axis angle q)
  ((foreign-lambda void "hpmRotateQuatAxisAngle"
     f32vector float f32vector)
   axis angle q)
  q)

(define (quaternion-x-rotation angle #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmXQuatRotation"
       float f32vector)
     angle res)))

(define (quaternion-rotate-x angle q)
  ((foreign-lambda void "hpmRotateQuatX"
     float f32vector)
   angle q)
  q)

(define (quaternion-y-rotation angle #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmYQuatRotation"
       float f32vector)
     angle res)))

(define (quaternion-rotate-y angle q)
  ((foreign-lambda void "hpmRotateQuatY"
     float f32vector)
   angle q)
  q)

(define (quaternion-z-rotation angle #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmZQuatRotation"
       float f32vector)
     angle res)))

(define (quaternion-rotate-Z angle q)
  ((foreign-lambda void "hpmRotateQuatZ"
     float f32vector)
   angle q)
  q)

(define (quaternion-ypr-rotation yaw pitch roll #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmYPRQuatRotation"
       float float float f32vector)
     yaw pitch roll res)))

(define (quaternion-rotate-ypr yaw pitch roll q)
  ((foreign-lambda void "hpmRotateQuatYPR"
     float float float f32vector)
   yaw pitch roll q)
  q)

(define (slerp a b t #!optional r)
  (let ((res (get-q-result r)))
    ((foreign-lambda void "hpmSlerp"
       f32vector f32vector float f32vector)
     a b t res)
    res))


;;; Matrix operations
(define-syntax bind-matrix-fun
  (ir-macro-transformer
   (lambda (exp rename compare)
     (match exp
       ((_ name c-name return . vars)
        (let* ((main-mat (first (last vars)))
               (other-vars (butlast vars))
               (result? (compare main-mat 'result))
               (pointer-name (symbol-append 'pointer- (strip-syntax name)))
               (vector-name (symbol-append 'f32vector- (strip-syntax name)))
               (types (map second other-vars))
               (pointer-types (map (lambda (t)
                                     (if (compare t 'f32vector)
                                         'c-pointer
                                         t))
                                   types))
               (vars (map first other-vars)))
          `(begin
             (define (,vector-name ,@vars
                                   ,@(if result?
                                         `(#!optional (,main-mat (make-f32vector 16)))
                                         `(,main-mat)))
               ((foreign-lambda ,return ,c-name ,@types f32vector)
                ,@vars ,main-mat)
               ,main-mat)
             (define (,pointer-name ,@vars ,main-mat)
               ((foreign-lambda ,return ,c-name ,@pointer-types c-pointer)
                ,@vars ,main-mat)
               ,main-mat)
             (define (,name ,@vars ,@(if result? '(#!optional) '())
                            ,main-mat)
               (cond
                ((pointer? ,main-mat) (,pointer-name ,@vars ,main-mat))
                ((f32vector? ,main-mat) (,vector-name ,@vars ,main-mat))
                ((boolean? ,main-mat) (,vector-name ,@vars
                                                    (make-f32vector 16 0 ,main-mat)))
                (else (error ',name "Wrong argument type" ,main-mat)))))))))))

(define (print-mat4 matrix)
  (define (vr i)
    (f32vector-ref matrix i))
  (define (pr i)
    (pointer-f32-ref (pointer+ matrix (* i 4))))
  (cond
   ((pointer? matrix)
    (format #t "[~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a]~%"
            (pr 0) (pr 4) (pr 8) (pr 12)
            (pr 1) (pr 5) (pr 9) (pr 13)
            (pr 2) (pr 6) (pr 10) (pr 14)
            (pr 3) (pr 7) (pr 11) (pr 15)))
   ((f32vector? matrix)
    (format #t "[~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a]~%"
            (vr 0) (vr 4) (vr 8) (vr 12)
            (vr 1) (vr 5) (vr 9) (vr 13)
            (vr 2) (vr 6) (vr 10) (vr 14)
            (vr 3) (vr 7) (vr 11) (vr 15)))
   (else (error 'print-mat4 "Wrong argument type" matrix))))

(bind-matrix-fun copy-mat4 "hpmCopyMat4" void
                 (source f32vector) (result f32vector))
(bind-matrix-fun m* "hpmMultMat4" void
                 (mat-a f32vector) (mat-b f32vector) (result f32vector))
(bind-matrix-fun mat4-identity "hpmIdentityMat4" void
                 (result f32vector))
(bind-matrix-fun translation "hpmTranslation" void
                 (v f32vector) (result f32vector))
(bind-matrix-fun translate "hpmTranslate" void
                 (v f32vector) (matrix f32vector))
(bind-matrix-fun x-rotation "hpmXRotation" void
                 (rotation float) (result f32vector))
(bind-matrix-fun rotate-x "hpmRotateX" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun y-rotation "hpmYRotation" void
                 (rotation float) (result f32vector))
(bind-matrix-fun rotate-y "hpmRotateY" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun z-rotation "hpmZRotation" void
                 (rotation float) (result f32vector))
(bind-matrix-fun rotate-z "hpmRotateZ" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun axis-angle-rotation "hpmAxisAngleRotation" void
                 (axis f32vector) (angle float) (result f32vector))
(bind-matrix-fun rotate-axis-angle "hpmRotateAxisAngle" void
                 (axis f32vector) (angle float) (matrix f32vector))
(bind-matrix-fun quaternion-rotation "hpmQuaternionRotation" void
                 (q f32vector) (result f32vector))
(bind-matrix-fun rotate-quaternion "hpmRotateQuaternion" void
                 (q f32vector) (matrix f32vector))
(bind-matrix-fun ypr-rotation "hpmYPRRotation" void
                 (yaw float) (pitch float) (roll float) (result f32vector))
(bind-matrix-fun rotate-ypr "hpmRotateYPR" void
                 (yaw float) (pitch float) (roll float) (matrix f32vector))
(bind-matrix-fun 2d-scaling "hpm2DScaling" void
                 (scale-x float) (scale-y float) (result f32vector))
(bind-matrix-fun scale-2d "hpmScale2D" void
                 (scale-x float) (scale-y float) (matrix f32vector))
(bind-matrix-fun 3d-scaling "hpm3DScaling" void
                 (scale-x float) (scale-y float) (scale-z float) (result f32vector))
(bind-matrix-fun scale-3d "hpmScale3D" void
                 (scale-x float) (scale-y float) (scale-z float) (matrix f32vector))
(bind-matrix-fun scaling "hpmScaling" void
                 (scale float) (result f32vector))
(bind-matrix-fun scale "hpmScale" void
                 (scale float) (matrix f32vector))
(bind-matrix-fun flip-x "hpmFlipY" void
                 (matrix f32vector))
(bind-matrix-fun flip-z "hpmFlipZ" void
                 (matrix f32vector))
(bind-matrix-fun translate-rotate-scale-2d "hpmTranslateRotateScale2D" void
                 (v f32vector) (rotate float) (scale float) 
                 (result f32vector))
(bind-matrix-fun transpose "hpmTranspose" void
                 (matrix f32vector) (result f32vector))
(bind-matrix-fun inverse "hpmInverse" void
                 (matrix f32vector) (result f32vector))
(bind-matrix-fun ortho "hpmOrtho" void
                 (width int) (height int) (near float) (far float)
                 (result f32vector))
(bind-matrix-fun frustum "hpmFrustum" void
                 (left float) (right float) (bottom float) (top float)
                 (near float) (far float)
                 (result f32vector))
(bind-matrix-fun perspective "hpmPerspective" void
                 (width int) (height int) (near float) (far float) (fov-angle float)
                 (result f32vector))
(bind-matrix-fun look-at "hpmLookAt" void
                 (eye f32vector) (obj f32vector) (up f32vector)
                 (result f32vector))
(bind-matrix-fun camera-inverse "hpmCameraInverse" void
                 (camera f32vector)
                 (result f32vector))

) ; module end
