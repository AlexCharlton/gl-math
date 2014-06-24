# gl-math
A very small math library for gamedev that mostly provides 4x4 float matrix operations. Uses the [hypermath](https://github.com/AlexCharlton/hypermath) library.

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install gl-math`.

## Requirements
- matchable

## Documentation
gl-math provides a number of functions for working with 4x4 matrices (plus a handful of others). The functionality is similar to what can be found in the [glm egg](http://wiki.call-cc.org/eggref/4/glm), but with some notable differences:

- Matrix functions only operate on 4x4 matrices
- Matrix functions can accept either f32vectors or pointers
- No container is used to represent matrices

Additionally, gl-math is one fifth the compiled size of glm, has a more straight-forward code-base, and complete documentation.

gl-math expects matrices to be f32vectors or pointers. f32vectors must be 16 elements long. The memory pointed to should likewise be an array of 16 floats. If a function accepts more than one matrix, all matrices must be of the same type.

gl-math operates on matrices in a column-major fashion in correspondence with OpenGL (e.g. translation components are at indices 12, 13, and 14).

### Matrix operations
    [procedure] (print-mat4 MATRIX)

Prints the given `MATRIX` to `(current-output-port)`.

    [procedure] (m* A B [RESULT])

Multiply matrix `A` by matrix `B`. If the matrix `RESULT` is given, it will be modified to contain the results of the multiplication. If `RESULT` is `#t`, `A` and `B` must be f32vectors and the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `A` and `B` must be f32vectors and the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (mat4-identity [RESULT])

Return an identity matrix. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (translation X Y Z [RESULT])
Return the translation matrix given by `X`, `Y`, and `Z`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (translate X Y Z MATRIX)

Translate `MATRIX` by `X`, `Y`, and `Z`.

    void hpmXRotation(float rotation, float *mat);
Create the rotation matrix of `rotation` radians around the X-axis in the given matrix.

    [procedure] (x-rotation ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the x-axis. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-x ANGLE MATRIX)

Rotate `MATRIX` around the x-axis by `ANGLE` radians.

    [procedure] (y-rotation ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the y-axis. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-y ANGLE MATRIX)

Rotate `MATRIX` around the y-axis by `ANGLE` radians.

    [procedure] (z-rotation ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the z-axis. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-z ANGLE MATRIX)

Rotate `MATRIX` around the z-axis by `ANGLE` radians.

    [procedure] (rotation X Y Z ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the vector `(X, Y, Z)`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate X Y Z ANGLE MATRIX)

Rotate `MATRIX` around the vector `(X, Y, Z)` by `ANGLE` radians.

    [procedure] (quaternion-rotation X Y Z W [RESULT])
Return the rotation matrix given by the quaternion `(X, Y, Z, Q)`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-quaternion X Y Z W MATRIX)

Rotate `MATRIX` by the quaternion `(X, Y, Z, Q)`.

    [procedure] (ypr-rotation YAW PITCH ROLL [RESULT])
Return the rotation matrix given by rotating by `ROLL` radians followed by `PITCH` radians followed by `YAW` radians. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-ypr YAW PITCH ROLL MATRIX)

.Rotate `MATRIX` by `ROLL` radians followed by `PITCH` radians followed by `YAW` radians.

    [procedure] (2d-scaling SCALE-X SCALE-Y [RESULT])

Return the matrix created by scaling the x and y axes by `SCALE-X` and `SCALE-Y`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (scale-2d SCALE-X SCALE-Y MATRIX)

Scale the x and y axis of `MATRIX` by `SCALE-X` and `SCALE-Y`.

    [procedure] (3d-scaling SCALE-X SCALE-Y SCALE-Z [RESULT])

Return the matrix created by scaling the x, y and z axes by `SCALE-X`, `SCALE-Y`, and `SCALE-Z`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (scale-3d SCALE-X SCALE-Y SCALE-Z MATRIX)

Scale the x, y, and z axis of `MATRIX` by `SCALE-X`, `SCALE-Y`, and `SCALE-Z`.

    [procedure] (scaling SCALE [RESULT])

Return the matrix created by scaling the x, y and z axes by `SCALE`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (scale SCALE MATRIX)

Scale the x, y, and z axis of `MATRIX` by `SCALE`.

    [procedure] (flip-x MATRIX)

Flip (mirror) `MATRIX` along the x-axis.

    [procedure] (flip-y MATRIX)

Flip (mirror) `MATRIX` along the y-axis.

    [procedure] (flip-z MATRIX)

Flip (mirror) `MATRIX` along the z-axis.

    [procedure] (translate-rotate-scale-2d X Y Z ANGLE SCALE [RESULT])

Efficiently create a matrix translated by `X`, `Y`, and `Z`, rotated around the z-axis by `ANGLE` radians, then scaled by `SCALE`. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (transpose MATRIX [RESULT])

Transpose `MATRIX`. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, `MATRIX` must be an f32vector and the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `MATRIX` must be an f32vector and the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (inverse MATRIX [RESULT])

Invert `MATRIX`. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, `MATRIX` must be an f32vector and the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `MATRIX` must be an f32vector and the returned value will be an f32vector located in normal garbage collected memory.

### Perspective matrices
    [procedure] (ortho WIDTH HEIGHT NEAR FAR [RESULT])

Create an orthographic projection matrix. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (perspective WIDTH HEIGHT NEAR FAR ANGLE [RESULT])

Create an perspective projection matrix. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (frustum LEFT RIGHT BOTTOM TOP NEAR FAR [RESULT])

Create a view-frustum matrix. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

### Camera functions
    [procedure] (look-at EYE-X EYE-Y EYE-Z X Y Z UP-X UP-Y UP-Z [RESULT])

Create a “look-at” style camera matrix. The camera is positioned at `(EYE-X, EYE-Y, EYE-Z)`, pointing towards `(X, Y, Z)`. `(UP-X, UP-Y, UP-Z)` defines the camera’s up vector. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (camera-inverse CAMERA [RESULT])

Invert `CAMERA` in an efficient fashion. This allows the camera to be constructed in an intuitive fashion by translating and rotating before inverting in order to position the scene properly. This function is far faster than the general `inverse` function, but the matrix `CAMERA` must only be a matrix representing a translation and a rotation (no scaling). If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, `CAMERA` must be an f32vector and the returned value will be an f32vector located in non-garbage collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `CAMERA` must be an f32vector and the returned value will be an f32vector located in normal garbage collected memory.

### Vector operations
    [procedure] (cross-product AX AY AZ BX BY BZ)

Return the result of the cross product between the vectors `(AX, AY, AZ)` and `(BX, BY, BZ)`. The resulting vector is returned as three values.

    [procedure] (dot-product AX AY AZ BX BY BZ)

Return the result of the dot product between the vectors `(AX, AY, AZ)` and `(BX, BY, BZ)`.

    [procedure] (normalize X Y Z)

Return the normalized vector `(X, Y, Z)`. The resulting vector is returned as three values.

    [procedure] (m*vector! MATRIX VECTOR)
Destructively multiply the 3 element f32vector `VECTOR` by `MATRIX`.

    [procedure] (m*vector-array! MATRIX VECTORS [STRIDE])
Destructively multiply the array of 3 element f32vectors `VECTORS` by `MATRIX`. The optional `STRIDE` specifies the number of elements between consecutive vectors. When `MATRIX` is a pointer, stride is given in number of bytes and must be at least 12. When `MATRIX` is an f32vector, stride is given in number of floats and must be at least 3.


### Angle operations
    [procedure] (degrees->radians ANGLE)

Converts `ANGLE` from degrees to radians.

    [procedure] (radians->degrees ANGLE)

Converts `ANGLE` from radians to degrees.

## Example

``` Scheme
(import chicken scheme)
(use gl-math)

(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define view-matrix
  (look-at 1 0 3
           0 0 0
           0 1 0))

(define model-matrix (mat4-identity))

(print-mat4 (m* projection-matrix
                (m* view-matrix model-matrix)))
```

## Version history
### Version 0.3.1
23 June 2014

* Matrix vector multiplication

### Version 0.2.0
21 June 2014

* Each transformation function now has two variants: one that initializes a matrix, and one that operates on a matrix
* Provide `pi/2`
* Provide quaternion and YPR rotation
* Remove unhelpful composite operations
* Fix optional arguments for matrix operations
* Fix a bug in `look-at`

### Version 0.1.0
17 June 2014

* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/gl-math).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## Licence
BSD
