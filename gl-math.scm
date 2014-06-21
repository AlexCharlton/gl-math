(module gl-math *

(import chicken scheme foreign srfi-1 extras)
(import-for-syntax matchable data-structures)
(use lolevel srfi-4)

#> #include "hypermath.h"
<#

(define-syntax bind-matrix-fun
  (ir-macro-transformer
   (lambda (exp rename compare)
     (match exp
       [(_ name c-name return . vars)
        (let* ([main-mat (first (last vars))]
               [other-vars (butlast vars)]
               [result? (compare main-mat 'result)]
               [pointer-name (symbol-append 'pointer- (strip-syntax name))]
               [vector-name (symbol-append 'f32vector- (strip-syntax name))]
               [types (map second other-vars)]
               [pointer-types (map (lambda (t)
                                     (if (compare t 'f32vector)
                                         'c-pointer
                                         t))
                                   types)]
               [vars (map first other-vars)])
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
                [(pointer? ,main-mat) (,pointer-name ,@vars ,main-mat)]
                [(f32vector? ,main-mat) (,vector-name ,@vars ,main-mat)]
                [(boolean? ,main-mat) (,vector-name ,@vars
                                                    (make-f32vector 16 0 ,main-mat))]
                [else (error ',name "Wrong argument type" ,main-mat)]))))]))))

(define (print-mat4 matrix)
  (define (vr i)
    (f32vector-ref matrix i))
  (define (pr i)
    (pointer-f32-ref (pointer+ matrix (* i 4))))
  (cond
   [(pointer? matrix)
    (format #t "[~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a]~%"
            (pr 0) (pr 4) (pr 8) (pr 12)
            (pr 1) (pr 5) (pr 9) (pr 13)
            (pr 2) (pr 6) (pr 10) (pr 14)
            (pr 3) (pr 7) (pr 11) (pr 15))]
   [[f32vector? matrix]
    (format #t "[~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a~% ~a ~a ~a ~a]~%"
            (vr 0) (vr 4) (vr 8) (vr 12)
            (vr 1) (vr 5) (vr 9) (vr 13)
            (vr 2) (vr 6) (vr 10) (vr 14)
            (vr 3) (vr 7) (vr 11) (vr 15))]
   [else (error print-mat4 "Wrong argument type" matrix)]))

(bind-matrix-fun m* "hpmMultMat4" void
                 (mat-a f32vector) (mat-b f32vector) (result f32vector))
(bind-matrix-fun mat4-identity "hpmIdentityMat4" void
                 (result f32vector))
(bind-matrix-fun translation "hpmTranslation" void
                 (x float) (y float) (z float) (matrix f32vector))
(bind-matrix-fun translate "hpmTranslate" void
                 (x float) (y float) (z float) (matrix f32vector))
(bind-matrix-fun x-rotation "hpmXRotation" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun rotate-x "hpmRotateX" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun y-rotation "hpmYRotation" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun rotate-y "hpmRotateY" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun z-rotation "hpmZRotation" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun rotate-z "hpmRotateZ" void
                 (rotation float) (matrix f32vector))
(bind-matrix-fun rotation "hpmRotation" void
                 (x float) (y float) (z float) (angle float) (matrix f32vector))
(bind-matrix-fun rotate "hpmRotate" void
                 (x float) (y float) (z float) (angle float) (matrix f32vector))
(bind-matrix-fun quaternion-rotation "hpmQuaternionRotation" void
                 (x float) (y float) (z float) (w float) (matrix f32vector))
(bind-matrix-fun rotate-quaternion "hpmRotateQuaternion" void
                 (x float) (y float) (z float) (w float) (matrix f32vector))
(bind-matrix-fun ypr-rotation "hpmYPRRotation" void
                 (yaw float) (pitch float) (roll float) (matrix f32vector))
(bind-matrix-fun rotate-ypr "hpmRotateYPR" void
                 (yaw float) (pitch float) (roll float) (matrix f32vector))
(bind-matrix-fun 2d-scaling "hpm2DScaling" void
                 (scale-x float) (scale-y float) (matrix f32vector))
(bind-matrix-fun scale-2d "hpmScale2D" void
                 (scale-x float) (scale-y float) (matrix f32vector))
(bind-matrix-fun 3d-scaling "hpm3DScaling" void
                 (scale-x float) (scale-y float) (scale-z float) (matrix f32vector))
(bind-matrix-fun scale-3d "hpmScale3D" void
                 (scale-x float) (scale-y float) (scale-z float) (matrix f32vector))
(bind-matrix-fun scaling "hpmScaling" void
                 (scale float) (matrix f32vector))
(bind-matrix-fun scale "hpmScale" void
                 (scale float) (matrix f32vector))
(bind-matrix-fun flip-x "hpmFlipY" void
                 (matrix f32vector))
(bind-matrix-fun flip-z "hpmFlipZ" void
                 (matrix f32vector))
(bind-matrix-fun translate-rotate-scale-2d "hpmTranslateRotateScale2D" void
                 (x float) (y float) (z float) (rotate float) (scale float) 
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
                 (eye-x float) (eye-y float) (eye-z float)
                 (x float) (y float) (z float)
                 (up-x float) (up-y float) (up-z float)
                 (result f32vector))
(bind-matrix-fun camera-inverse "hpmCameraInverse" void
                 (camera f32vector)
                 (result f32vector))

(define (cross-product ax ay az bx by bz)
  (let-location ([rx float] [ry float] [rz float])
    ((foreign-lambda void "hpmCross" float float float float float float
                     (c-pointer float) (c-pointer float) (c-pointer float))
     ax ay az bx by bz #$rx #$ry #$rz)
    (values rx ry rz)))

(define (normalize x y z)
  (let-location ([rx float] [ry float] [rz float])
    ((foreign-lambda void "hpmNormalize" float float float
                     (c-pointer float) (c-pointer float) (c-pointer float))
     x y z #$rx #$ry #$rz)
    (values rx ry rz)))

(define dot-product
  (foreign-lambda float "hpmDot"
                  float float float float float float))

(define degrees->radians
  (foreign-lambda float "hpmDegreesToRadians" float))

(define radians->degrees
  (foreign-lambda float "hpmRadiansToDegrees" float))

(define pi 3.14159265358979)
(define pi/2 (/ pi 2))

) ; module end
