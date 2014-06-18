#ifndef HYPERMATH
#define HYPERMATH 1

float hpmDegreesToRadians(float deg);

float hpmRadiansToDegrees(float rad);

void hpmMultMat4(const float *matA, const float *matB, float *result);

void hpmPrintMat4(const float *m);

void hpmIdentityMat4(float *m);

void hpmTranslate(float x, float y, float z, float *mat);

void hpmRotateX(float rotation, float *mat);

void hpmRotateY(float rotation, float *mat);

void hpmRotateZ(float rotation, float *mat);

void hpmRotate(float x, float y, float z, float angle, float *mat);

void hpmScale2D(float scaleX, float scaleY, float *mat);

void hpmScale3D(float scaleX, float scaleY, float scaleZ, float *mat);

void hpmScale(float factor, float *mat);

void hpmFlipX(float *mat);

void hpmFlipY(float *mat);

void hpmFlipZ(float *mat);

void hpmTranslateScale(float x, float y, float z, float scale, float *mat);

void hpmTranslateRotateScale2D(float x, float y, float z, float angle, float scale,
                               float *mat);

void hpmTranslateRotateScale(float x, float y, float z, 
			     float rx, float ry, float rz, float angle,
                             float scale, float *mat);

void hpmTranspose(const float *mat, float *result);

void hpmInverse(const float *mat, float *result);

// Vector operations
void hpmCross(float ax, float ay, float az, float bx, float by, float bz, float *rx, float *ry, float *rz);

void hpmNormalize(float x, float y, float z, float *rx, float *ry, float *rz);

float hpmDot(float ax, float ay, float az, float bx, float by, float bz);

// Projection
void hpmOrtho(int width, int height, float near, float far, float *mat);

void hpmFrustum(float left, float right, float bottom, float top,
		float near, float far, float *mat);

void hpmPerspective(int width, int height, float near, float far, float angle,
		    float *mat);

// Camera
void hpmLookAt(float eyeX, float eyeY, float eyeZ, float x, float y, float z, float upX, float upY, float upZ, float *mat);

void hpmCameraInverse(const float *camera, float *inverse);

#endif
