# gl-math

A small math library aimed at gamedev that provides 4x4 float matrix, vector, and quaternion operations. Uses the [hypermath](https://github.com/AlexCharlton/hypermath) library.

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

gl-math expects matrices, vectors, and quaternions to be f32vectors or pointers. f32vectors must be 16 elements long, 3 elements long, or 4 elements long for matrices, vectors, or quaternions, respectively. The memory pointed to should likewise be an array of 16, 3, or 4 floats . If a function accepts more than one matrix, vector, or quaternion, all must be of the same type.

gl-math operates on matrices in a column-major fashion in correspondence with OpenGL (e.g. translation components are at indices 12, 13, and 14). Vectors are arranged as (`(x y z)`), and quaternions as (`(x y z w)`).

### Matrix operations
    [procedure] (print-mat4 MATRIX)

Prints the given `MATRIX` to `(current-output-port)`.

    [procedure] (copy-mat4 MATRIX [RESULT])

Make a copy of `MATRIX`. If the matrix `RESULT` is given, it will be modified to contain the contents of `MATRIX`. If `RESULT` is `#t`, `MATRIX` must be an f32vector and the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `MATRIX` must be an f32vector and the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (m* A B [RESULT])

Multiply matrix `A` by matrix `B`. If the matrix `RESULT` is given, it will be modified to contain the results of the multiplication. If `RESULT` is `#t`, `A` and `B` must be f32vectors and the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `A` and `B` must be f32vectors and the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (mat4-identity [RESULT])

Return an identity matrix. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (translation VECTOR [RESULT])
Return the translation matrix given by `VECTOR`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (translate VECTOR MATRIX)

Translate `MATRIX` by `VECTOR`.

    [procedure] (x-rotation ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the x-axis. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-x ANGLE MATRIX)

Rotate `MATRIX` around the x-axis by `ANGLE` radians.

    [procedure] (y-rotation ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the y-axis. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-y ANGLE MATRIX)

Rotate `MATRIX` around the y-axis by `ANGLE` radians.

    [procedure] (z-rotation ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the z-axis. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-z ANGLE MATRIX)

Rotate `MATRIX` around the z-axis by `ANGLE` radians.

    [procedure] (axis-angle-rotation AXIS ANGLE [RESULT])
Return the rotation matrix given by a rotation of `ANGLE` radians around the vector `AXIS`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-axis-angle AXIS ANGLE MATRIX)

Rotate `MATRIX` around the vector `AXIS` by `ANGLE` radians.

    [procedure] (quaternion-rotation Q [RESULT])
Return the rotation matrix given by the quaternion `Q`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-quaternion Q MATRIX)

Rotate `MATRIX` by the quaternion `Q`.

    [procedure] (ypr-rotation YAW PITCH ROLL [RESULT])
Return the rotation matrix given by rotating by `ROLL` radians around the z-axis followed by `PITCH` radians around the x-axis followed by `YAW` radians around the y-axis. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (rotate-ypr YAW PITCH ROLL MATRIX)

Rotate `MATRIX` by `ROLL` radians around the z-axis followed by `PITCH` radians around the x-axis followed by `YAW` radians around the y-axis.

    [procedure] (2d-scaling SCALE-X SCALE-Y [RESULT])

Return the matrix created by scaling the x and y axes by `SCALE-X` and `SCALE-Y`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (scale-2d SCALE-X SCALE-Y MATRIX)

Scale the x and y axis of `MATRIX` by `SCALE-X` and `SCALE-Y`.

    [procedure] (3d-scaling SCALE-X SCALE-Y SCALE-Z [RESULT])

Return the matrix created by scaling the x, y and z axes by `SCALE-X`, `SCALE-Y`, and `SCALE-Z`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (scale-3d SCALE-X SCALE-Y SCALE-Z MATRIX)

Scale the x, y, and z axis of `MATRIX` by `SCALE-X`, `SCALE-Y`, and `SCALE-Z`.

    [procedure] (scaling SCALE [RESULT])

Return the matrix created by scaling the x, y and z axes by `SCALE`. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (scale SCALE MATRIX)

Scale the x, y, and z axis of `MATRIX` by `SCALE`.

    [procedure] (flip-x MATRIX)

Flip (mirror) `MATRIX` along the x-axis.

    [procedure] (flip-y MATRIX)

Flip (mirror) `MATRIX` along the y-axis.

    [procedure] (flip-z MATRIX)

Flip (mirror) `MATRIX` along the z-axis.

    [procedure] (translate-rotate-scale-2d VECTOR ANGLE SCALE [RESULT])

Efficiently create a matrix translated by `VECTOR`, rotated around the z-axis by `ANGLE` radians, then scaled by `SCALE`. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (transpose MATRIX [RESULT])

Transpose `MATRIX`. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, `MATRIX` must be an f32vector and the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `MATRIX` must be an f32vector and the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (inverse MATRIX [RESULT])

Invert `MATRIX`. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, `MATRIX` must be an f32vector and the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `MATRIX` must be an f32vector and the returned value will be an f32vector located in normal garbage collected memory.

### Perspective matrices
    [procedure] (ortho WIDTH HEIGHT NEAR FAR [RESULT])

Create an orthographic projection matrix. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (perspective WIDTH HEIGHT NEAR FAR ANGLE [RESULT])

Create an perspective projection matrix. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (frustum LEFT RIGHT BOTTOM TOP NEAR FAR [RESULT])

Create a view-frustum matrix. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

### Camera functions
    [procedure] (look-at EYE OBJ UP [RESULT])

Create a “look-at” style camera matrix. The camera is positioned at point `EYE`, pointing towards the point `OBJ`. `UP` defines the camera’s up vector. If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (camera-inverse CAMERA [RESULT])

Invert `CAMERA` in an efficient fashion. This allows the camera to be constructed in an intuitive fashion by translating and rotating before inverting in order to position the scene properly. This function is far faster than the general `inverse` function, but the matrix `CAMERA` must only be a matrix representing a translation and a rotation (no scaling). If the matrix `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t`, `CAMERA` must be an f32vector and the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, `CAMERA` must be an f32vector and the returned value will be an f32vector located in normal garbage collected memory.

### Vector operations
    [procedure] (make-point X Y Z [NON-GC?])
    [procedure] (point-x POINT)
    [procedure] (point-y POINT)
    [procedure] (point-z POINT)
    [procedure] (point-x-set! POINT)
    [procedure] (point-y-set! POINT)
    [procedure] (point-z-set! POINT)

Vector constructor, getters, and setters. If `NON-GC` is `#t`, the point is created in a non-garbage-collected area (the memory will still be freed when there are no more references to the vector). 

    [procedure] (v+ A B [RESULT])
 
Return the result of the addition of vectors `A` and `B`. If the vector `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (v- A B [RESULT])
 
Return the result of the subtraction of vector `B` from `A`. If the vector `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (v* V S [RESULT])
 
Return the result of the multiplication of vector `A` with scalar `S`. If the vector `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (cross-product A B [RESULT])
 
Return the result of the cross product between the vectors `A` and `B`. If the vector `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (dot-product A B)

Return the result of the dot product between the vectors `A` and `B`.

    [procedure] (vector-magnitude V)

Return the magnitude of vector `V`.

    [procedure] (normalize! V)

Destructively normalize the vector `V`.

    [procedure] (m*vector! MATRIX VECTOR)

Destructively multiply `VECTOR` by `MATRIX`.

    [procedure] (m*vector-array! MATRIX VECTORS stride: [STRIDE] length: [LENGTH])

Destructively multiply the array of 3 element floats `VECTORS` by `MATRIX`. `VECTORS` may be given as an f32vector or a pointer. The keyword `STRIDE` specifies the number of elements between consecutive vectors, given in number of floats (which must be at least 3) when `VECTORS` is an f32vector and in bytes when `VECTORS` is a pointer. When `VECTORS` is given as a pointer, the keyword `LENGTH` must be provided, specifying the number of vectors in `VECTORS`.

    [procedure] (lerp A B T [RESULT])

Linear interpolation between the points `A` and `B` with the interpolation parameter `T` which must be between 0 and 1. If the vector `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.


### Quaternion operations
Quaternions are expected to be normalized before they are used in certain functions (`quaternion-normalize!` may be used to do so). All the provided functions that create quaternions, create unit quaternions. 

The order of quaternion cross-multiplication is the inverse of the “standard” order, so a quaternion that has undergone a series or rotations will represent the same rotation as a marix that has gone through the same series, in the same order.

    [procedure] (make-quaternion X Y Z W [NON-GC?])
    [procedure] (quaternion-x POINT)
    [procedure] (quaternion-y POINT)
    [procedure] (quaternion-z POINT)
    [procedure] (quaternion-w POINT)
    [procedure] (quaternion-x-set! POINT)
    [procedure] (quaternion-y-set! POINT)
    [procedure] (quaternion-z-set! POINT)
    [procedure] (quaternion-w-set! POINT)

Quaternion constructor, getters, and setters. If `NON-GC` is `#t`, the quaternion is created in a non-garbage-collected area (the memory will still be freed when there are no more references to the quaternion). 

    [procedure] (quaternion-normalize! Q)

Destructively normalize the quaternion `Q`.

    [procedure] (quaternion-inverse Q [RESULT])

Return the inverse of the unit quaternion `Q`. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (quaternion-cross-product A B [RESULT])

Return the cross-product of the quaternions `A` and `B`. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (quaternion-axis-angle-rotation AXIS ANGLE [RESULT])

Return the quaternion corresponding to a rotation of `ANGLE` radians around the vector `AXIS`. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (quaternion-rotate-axis-angle AXIS ANGLE Q)

Rotate the quaternion `Q` by a rotation of `ANGLE` radians around the vector `AXIS`.

    [procedure] (quaternion-x-rotation ANGLE [RESULT])

Return the quaternion corresponding to a rotation of `ANGLE` radians around the x-axis. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (quaternion-rotate-x ANGLE Q)

Rotate the quaternion `Q` by a rotation of `ANGLE` radians around the x-axis.

    [procedure] (quaternion-y-rotation ANGLE [RESULT])

Return the quaternion corresponding to a rotation of `ANGLE` radians around the y-axis. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (quaternion-rotate-y ANGLE Q)

Rotate the quaternion `Q` by a rotation of `ANGLE` radians around the y-axis.

    [procedure] (quaternion-z-rotation ANGLE [RESULT])

Return the quaternion corresponding to a rotation of `ANGLE` radians around the z-axis. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (quaternion-rotate-z ANGLE Q)

Rotate the quaternion `Q` by a rotation of `ANGLE` radians around the z-axis.

    [procedure] (quaternion-ypr-rotation YAW PITCH ROLL [RESULT])

Return the quaternion corresponding to a rotation of `ROLL` radians around the z-axis followed by `PITCH` radians around the x-axis followed by `YAW` radians around the y-axis. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.

    [procedure] (quaternion-rotate-ypr YAW PITCH ROLL Q)

Rotate the quaternion `Q` by `ROLL` radians around the z-axis followed by `PITCH` radians around the x-axis followed by `YAW` radians around the y-axis.

    [procedure] (quaternion-rotate-point! Q P)

Destructively rotate the point `P` by the unit quaternion `Q`.

    [procedure] (slerp A B T [RESULT])

Spherical linear interpolation between the quaternions `A` and `B` with the interpolation parameter `T` which must be between 0 and 1. If the quaternion `RESULT` is given, it will be modified to contain the result. If `RESULT` is `#t` the returned value will be an f32vector located in non-garbage-collected memory (the memory will still be freed when there are no more references to the matrix). If `RESULT` is not provided, the returned value will be an f32vector located in normal garbage collected memory.


### Angle operations
    [procedure] (degrees->radians ANGLE)

Converts `ANGLE` from degrees to radians.

    [procedure] (radians->degrees ANGLE)

Converts `ANGLE` from radians to degrees.

    [constant] pi
    [constant] pi/2

## Example

``` Scheme
(import chicken scheme)
(use gl-math)

(define projection-matrix
  (perspective 640 480 0.1 100 70))

(define view-matrix
  (look-at (make-point 1 0 3)
           (make-point 0 0 0)
           (make-point 0 1 0)))

(define model-matrix (mat4-identity))

(print-mat4 (m* projection-matrix
                (m* view-matrix model-matrix)))
```

## Version history
### Version 0.5.2
10 September 2014

* `m*vector-array!`: Stride is given in bytes when vector is a pointer

**Version 0.5.0**

2 September 2014

* Many new vector and quaternion functions
* Functions that previously accepted vectors as individual floats, now accept them as f32vectors

### Version 0.4.1
30 August 2014

* Fix `m*vector-array!`

**Version 0.4.0**

27 July 2014

* Add `copy-mat4`

### Version 0.3.2
21 July 2014

* Allow pointer to array of vectors to be passed to `m*vector-array!`
* Fix error forms

**Version 0.3.1**

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
