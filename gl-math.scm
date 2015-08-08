(module gl-math *

(import chicken scheme foreign srfi-1 extras)
(import-for-syntax matchable data-structures)
(use lolevel srfi-4)

(foreign-declare "#include <hypermath.h>")

(define-syntax bind-math-fun
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
               (ret (if (compare return 'void)
                        (list main-mat)
                        '()))
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
               ,@ret)
             (define (,pointer-name ,@vars ,main-mat)
               ((foreign-lambda ,return ,c-name ,@pointer-types c-pointer)
                ,@vars ,main-mat)
               ,@ret)
             (define (,name ,@vars ,@(if result? '(#!optional) '())
                            ,main-mat)
               (cond
                ((pointer? ,main-mat) (,pointer-name ,@vars ,main-mat))
                ((f32vector? ,main-mat) (,vector-name ,@vars ,main-mat))
                ((boolean? ,main-mat) (,vector-name ,@vars
                                                    (make-f32vector 16 0 ,main-mat)))
                (else (error ',name "Wrong argument type" ,main-mat)))))))))))


;;; Angle operations
(define degrees->radians
  (foreign-lambda float "hpmDegreesToRadians" float))

(define radians->degrees
  (foreign-lambda float "hpmRadiansToDegrees" float))

(define pi 3.1415926535897932384626433832795028842)
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
  (cond
   ((f32vector? vector)
    (when (and (< stride 3) (not (zero? stride)))
      (error 'm*vector-array! "Stride must be at least 3 when vector is an f32vector" stride))
    ((cond
      ((f32vector? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" f32vector f32vector size_t size_t))
      ((pointer? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" c-pointer f32vector size_t size_t))
      (else (error 'm*vector-array! "Wrong argument type" matrix)))
     matrix vector (quotient (f32vector-length vector)
                             (if (zero? stride) 3 stride))
     (* stride 4)))
   ((u8vector? vector)
    (when (and (< stride 12) (not (zero? stride)))
      (error 'm*vector-array! "Stride must be at least 12 when vector is a u8vector" stride))
    ((cond
      ((f32vector? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" f32vector u8vector size_t size_t))
      ((pointer? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" c-pointer u8vector size_t size_t))
      (else (error 'm*vector-array! "Wrong argument type" matrix)))
     matrix vector (quotient (u8vector-length vector)
                             (if (zero? stride) 12 stride))
     stride))
   ((pointer? vector)
    (when (and (< stride 12) (not (zero? stride)))
      (error 'm*vector-array! "Stride must be at least 12 when vector is a pointer" stride))
    (when (< length 1)
      (error 'm*vector-array! "length must be given (and positive) when vector is a pointer" length))
    ((cond
      ((f32vector? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" f32vector c-pointer size_t size_t))
      ((pointer? matrix)
       (foreign-lambda void "hpmMat4VecArrayMult" c-pointer c-pointer size_t size_t))
      (else (error 'm*vector-array! "Wrong argument type" matrix)))
     matrix vector length stride))
   (else (error 'm*vector-array! "Wrong argument type" vector)))
  vector)

(bind-math-fun cross-product "hpmCross" void
               (a f32vector) (b f32vector) (result f32vector))

(bind-math-fun v+ "hpmAddVec" void
               (a f32vector) (b f32vector) (result f32vector))

(bind-math-fun v- "hpmSubVec" void
               (a f32vector) (b f32vector) (result f32vector))

(bind-math-fun v* "hpmMultVec" void
               (v f32vector) (s float) (result f32vector))

(bind-math-fun vector-magnitude "hpmMagnitude" float
               (v f32vector))

(bind-math-fun normalize! "hpmNormalize" void
               (v f32vector))

(bind-math-fun dot-product "hpmDot" float
               (a f32vector) (b f32vector))

(bind-math-fun lerp "hpmLerp" void
               (a f32vector) (b f32vector) (t float) (result f32vector))

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

(bind-math-fun quaternion-normalize! "hpmQuatNormalize" void
               (q f32vector))

(bind-math-fun quaternion-inverse "hpmQuatInverse" void
               (q f32vector) (result f32vector))

(bind-math-fun quaternion-cross-product "hpmQuatCross" void
               (a f32vector) (b f32vector) (result f32vector))

(bind-math-fun quaternion-rotate-point! "hpmQuatVecRotate" void
               (q f32vector) (p f32vector))

(bind-math-fun quaternion-axis-angle-rotation "hpmAxisAngleQuatRotation" void
               (axis f32vector) (angle float) (result f32vector))

(bind-math-fun quaternion-rotate-axis-angle "hpmRotateQuatAxisAngle" void
               (axis f32vector) (angle float) (q f32vector))

(bind-math-fun quaternion-x-rotation "hpmXQuatRotation" void
               (angle float) (result f32vector))

(bind-math-fun quaternion-rotate-x "hpmRotateQuatX" void
               (angle float) (q f32vector))

(bind-math-fun quaternion-y-rotation "hpmYQuatRotation" void
               (angle float) (result f32vector))

(bind-math-fun quaternion-rotate-y "hpmRotateQuatY" void
               (angle float) (q f32vector))

(bind-math-fun quaternion-z-rotation "hpmZQuatRotation" void
               (angle float) (result f32vector))

(bind-math-fun quaternion-rotate-z "hpmRotateQuatZ" void
               (angle float) (q f32vector))

(bind-math-fun quaternion-ypr-rotation "hpmYPRQuatRotation" void
               (yaw float) (pitch float) (roll float) (result f32vector))

(bind-math-fun quaternion-rotate-ypr "hpmRotateQuatYPR" void
               (yaw float) (pitch float) (roll float) (q f32vector))

(bind-math-fun slerp "hpmSlerp" void
               (a f32vector) (b f32vector) (t float) (result f32vector))


;;; Matrix operations
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

(bind-math-fun copy-mat4 "hpmCopyMat4" void
                 (source f32vector) (result f32vector))
(bind-math-fun m* "hpmMultMat4" void
                 (mat-a f32vector) (mat-b f32vector) (result f32vector))
(bind-math-fun m*s "hpmMultMat4S" void
               (mat-a f32vector) (s float) (result f32vector))
(bind-math-fun m+ "hpmAddMat4" void
               (mat-a f32vector) (mat-b f32vector) (result f32vector))
(bind-math-fun m- "hpmSubMat4" void
                 (mat-a f32vector) (mat-b f32vector) (result f32vector))
(bind-math-fun mat4-identity "hpmIdentityMat4" void
                 (result f32vector))
(bind-math-fun translation "hpmTranslation" void
                 (v f32vector) (result f32vector))
(bind-math-fun translate "hpmTranslate" void
                 (v f32vector) (matrix f32vector))
(bind-math-fun x-rotation "hpmXRotation" void
                 (rotation float) (result f32vector))
(bind-math-fun rotate-x "hpmRotateX" void
                 (rotation float) (matrix f32vector))
(bind-math-fun y-rotation "hpmYRotation" void
                 (rotation float) (result f32vector))
(bind-math-fun rotate-y "hpmRotateY" void
                 (rotation float) (matrix f32vector))
(bind-math-fun z-rotation "hpmZRotation" void
                 (rotation float) (result f32vector))
(bind-math-fun rotate-z "hpmRotateZ" void
                 (rotation float) (matrix f32vector))
(bind-math-fun axis-angle-rotation "hpmAxisAngleRotation" void
                 (axis f32vector) (angle float) (result f32vector))
(bind-math-fun rotate-axis-angle "hpmRotateAxisAngle" void
                 (axis f32vector) (angle float) (matrix f32vector))
(bind-math-fun quaternion-rotation "hpmQuaternionRotation" void
                 (q f32vector) (result f32vector))
(bind-math-fun rotate-quaternion "hpmRotateQuaternion" void
                 (q f32vector) (matrix f32vector))
(bind-math-fun ypr-rotation "hpmYPRRotation" void
                 (yaw float) (pitch float) (roll float) (result f32vector))
(bind-math-fun rotate-ypr "hpmRotateYPR" void
                 (yaw float) (pitch float) (roll float) (matrix f32vector))
(bind-math-fun 2d-scaling "hpm2DScaling" void
                 (scale-x float) (scale-y float) (result f32vector))
(bind-math-fun scale-2d "hpmScale2D" void
                 (scale-x float) (scale-y float) (matrix f32vector))
(bind-math-fun 3d-scaling "hpm3DScaling" void
                 (scale-x float) (scale-y float) (scale-z float) (result f32vector))
(bind-math-fun scale-3d "hpmScale3D" void
                 (scale-x float) (scale-y float) (scale-z float) (matrix f32vector))
(bind-math-fun scaling "hpmScaling" void
                 (scale float) (result f32vector))
(bind-math-fun scale "hpmScale" void
                 (scale float) (matrix f32vector))
(bind-math-fun flip-x "hpmFlipX" void
                 (matrix f32vector))
(bind-math-fun flip-y "hpmFlipY" void
                 (matrix f32vector))
(bind-math-fun flip-z "hpmFlipZ" void
                 (matrix f32vector))
(bind-math-fun translate-rotate-scale-2d "hpmTranslateRotateScale2D" void
                 (v f32vector) (rotate float) (scale float) 
                 (result f32vector))
(bind-math-fun transpose "hpmTranspose" void
                 (matrix f32vector) (result f32vector))
(bind-math-fun inverse "hpmInverse" void
                 (matrix f32vector) (result f32vector))
(bind-math-fun fast-inverse-transpose "hpmFastInverseTranspose" void
                 (matrix f32vector) (result f32vector))
(bind-math-fun ortho "hpmOrtho" void
                 (width int) (height int) (near float) (far float)
                 (result f32vector))
(bind-math-fun ortho-viewport "hpmOrthoViewport" void
                 (left float) (right float) (bottom float) (top float)
                 (near float) (far float)
                 (viewport-left float) (viewport-right float)
                 (viewport-bottom float) (viewport-top float)
                 (result f32vector))
(bind-math-fun frustum "hpmFrustum" void
                 (left float) (right float) (bottom float) (top float)
                 (near float) (far float)
                 (result f32vector))
(bind-math-fun frustum-viewport "hpmFrustumViewport" void
                 (left float) (right float) (bottom float) (top float)
                 (near float) (far float)
                 (viewport-left float) (viewport-right float)
                 (viewport-bottom float) (viewport-top float)
                 (result f32vector))
(bind-math-fun perspective "hpmPerspective" void
                 (width int) (height int) (near float) (far float) (fov-angle float)
                 (result f32vector))
(bind-math-fun look-at "hpmLookAt" void
                 (eye f32vector) (obj f32vector) (up f32vector)
                 (result f32vector))
(bind-math-fun camera-inverse "hpmCameraInverse" void
                 (camera f32vector)
                 (result f32vector))

) ; module end
