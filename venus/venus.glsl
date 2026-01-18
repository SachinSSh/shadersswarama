// Venus Sulfuric Wasteland - VISIBLE OCEAN WAVES
// Thick clouds, BIG visible waves and ripples
// 2024

#define DRAG_MULT 0.05
#define ACID_DEPTH 1.5  // DEEPER for more visible waves
#define CAMERA_HEIGHT 2.0
#define ITERATIONS_RAYMARCH 12
#define ITERATIONS_NORMAL 20

#define NormalizedMouse (iMouse.xy / iResolution.xy)

// ============ NOISE FUNCTIONS ============

float hash(float n) {
    return fract(sin(n) * 43758.5453);
}

float hash2(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

float hash3(vec3 p) {
    return fract(sin(dot(p, vec3(127.1, 311.7, 74.7))) * 43758.5453);
}

float noise2D(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    
    float a = hash2(i);
    float b = hash2(i + vec2(1.0, 0.0));
    float c = hash2(i + vec2(0.0, 1.0));
    float d = hash2(i + vec2(1.0, 1.0));
    
    return mix(mix(a, b, f.x), mix(c, d, f.x), f.y);
}

float noise3D(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    
    float n = i.x + i.y * 157.0 + 113.0 * i.z;
    return mix(
        mix(mix(hash(n), hash(n + 1.0), f.x),
            mix(hash(n + 157.0), hash(n + 158.0), f.x), f.y),
        mix(mix(hash(n + 113.0), hash(n + 114.0), f.x),
            mix(hash(n + 270.0), hash(n + 271.0), f.x), f.y),
        f.z);
}

float fbm2D(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;
    for(int i = 0; i < 6; i++) {
        value += amplitude * noise2D(p * frequency);
        amplitude *= 0.5;
        frequency *= 2.0;
    }
    return value;
}

float fbm3D(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for(int i = 0; i < 5; i++) {
        value += amplitude * noise3D(p);
        amplitude *= 0.5;
        p *= 2.0;
    }
    return value;
}

float turbulence3D(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for(int i = 0; i < 6; i++) {
        value += amplitude * abs(noise3D(p) * 2.0 - 1.0);
        amplitude *= 0.5;
        p *= 2.0;
    }
    return value;
}

// ============ CLOUD FUNCTIONS (KEEPING SAME) ============

float cloudPuff(vec2 uv, vec2 center, float size) {
    float d = length(uv - center);
    return smoothstep(size, size * 0.2, d);
}

vec3 renderClouds(vec3 rayDir, float time) {
    vec3 darkCloud = vec3(0.25, 0.18, 0.08);
    vec3 midCloud = vec3(0.50, 0.38, 0.18);
    vec3 brightCloud = vec3(0.70, 0.55, 0.30);
    vec3 sulfurTint = vec3(0.65, 0.55, 0.20);
    
    float clouds = 0.0;
    float cloudDetail = 0.0;
    
    float horizon = 1.0 - abs(rayDir.y);
    vec2 uv = rayDir.xz / (abs(rayDir.y) + 0.1);
    
    vec2 cloudUV1 = uv * 0.15 + time * 0.002;
    float bigCloud = fbm2D(cloudUV1);
    bigCloud = smoothstep(0.3, 0.7, bigCloud);
    clouds += bigCloud * 1.0;
    
    vec2 cloudUV2 = uv * 0.3 + vec2(time * 0.003, -time * 0.001);
    float medCloud = fbm2D(cloudUV2 + 50.0);
    medCloud = smoothstep(0.35, 0.65, medCloud);
    clouds += medCloud * 0.7;
    
    vec2 cloudUV3 = uv * 0.6 - time * 0.002;
    float detailCloud = fbm2D(cloudUV3 + 100.0);
    detailCloud = smoothstep(0.4, 0.6, detailCloud);
    cloudDetail = detailCloud;
    clouds += detailCloud * 0.4;
    
    vec2 cloudUV4 = uv * 1.2 + time * 0.001;
    float smallCloud = fbm2D(cloudUV4 + 200.0);
    smallCloud = pow(smallCloud, 1.5);
    clouds += smallCloud * 0.3;
    
    vec2 streakUV = uv * vec2(0.5, 2.0) + time * 0.001;
    float streaks = fbm2D(streakUV);
    streaks = pow(streaks, 2.0) * 0.5;
    clouds += streaks * 0.25;
    
    for(int i = 0; i < 8; i++) {
        float fi = float(i);
        vec2 puffPos = vec2(
            sin(fi * 1.7 + time * 0.01) * 3.0,
            cos(fi * 2.3 - time * 0.008) * 2.0
        );
        float puff = cloudPuff(uv * 0.3, puffPos, 0.5 + sin(fi) * 0.2);
        puff *= fbm2D(uv * 2.0 + fi);
        clouds += puff * 0.3;
    }
    
    clouds *= 1.0 + horizon * 0.5;
    clouds = clamp(clouds, 0.0, 1.5);
    
    vec3 cloudColor = mix(darkCloud, midCloud, clouds * 0.7);
    cloudColor = mix(cloudColor, brightCloud, pow(clouds, 2.0) * 0.5);
    cloudColor = mix(cloudColor, sulfurTint, cloudDetail * 0.3);
    
    float shadow = fbm2D(uv * 0.4 + 30.0);
    cloudColor *= 0.7 + shadow * 0.4;
    
    return vec3(clouds, clouds, 1.0) * cloudColor;
}

vec3 renderSmoke(vec3 rayDir, vec2 screenUV, float time) {
    vec3 smokeColor = vec3(0.30, 0.22, 0.10);
    vec3 thickSmoke = vec3(0.20, 0.14, 0.06);
    
    float smoke = 0.0;
    
    float horizon = pow(1.0 - abs(rayDir.y), 2.0);
    vec2 uv = rayDir.xz / (abs(rayDir.y) + 0.15);
    
    vec3 smokePos1 = vec3(uv * 0.3, time * 0.02);
    smokePos1.y -= time * 0.03;
    float smoke1 = turbulence3D(smokePos1);
    smoke += smoke1 * 0.6;
    
    vec3 smokePos2 = vec3(uv * 0.5 + 10.0, time * 0.015);
    smokePos2.x += sin(time * 0.02) * 2.0;
    float smoke2 = turbulence3D(smokePos2);
    smoke += smoke2 * 0.4;
    
    for(int i = 0; i < 5; i++) {
        float fi = float(i);
        vec2 colPos = vec2(sin(fi * 2.5) * 4.0, cos(fi * 1.8) * 4.0);
        vec2 diff = uv * 0.4 - colPos;
        float dist = length(diff);
        
        vec3 colSmokePos = vec3(diff, time * 0.04 + fi);
        colSmokePos.y -= time * 0.05;
        
        float column = exp(-dist * 0.8);
        column *= turbulence3D(colSmokePos * 1.5);
        smoke += column * 0.4;
    }
    
    vec2 smogUV = uv * 0.2;
    float smog = fbm2D(smogUV + time * 0.002);
    smog += fbm2D(smogUV * 2.0 - time * 0.001) * 0.5;
    smoke += smog * horizon * 0.8;
    
    vec3 screenSmokePos = vec3(screenUV * 3.0, time * 0.01);
    float screenSmoke = turbulence3D(screenSmokePos);
    smoke += screenSmoke * 0.3;
    
    float angle = atan(rayDir.z, rayDir.x);
    float swirl = sin(angle * 3.0 + time * 0.03 + smoke * 2.0);
    smoke *= 0.9 + swirl * 0.2;
    
    smoke = clamp(smoke, 0.0, 1.2);
    
    vec3 finalSmoke = mix(smokeColor, thickSmoke, smoke * 0.5);
    finalSmoke *= smoke;
    
    return finalSmoke;
}

// ============ COMPLETELY NEW WAVE SYSTEM ============

// Big rolling waves - very visible
float bigWaves(vec2 pos, float time) {
    float waves = 0.0;
    
    // PRIMARY LARGE WAVES - these create the up/down motion
    waves += sin(pos.x * 0.15 + time * 0.08) * 0.6;
    waves += sin(pos.y * 0.12 - time * 0.06) * 0.5;
    waves += sin((pos.x + pos.y) * 0.10 + time * 0.05) * 0.4;
    waves += sin((pos.x - pos.y) * 0.08 - time * 0.04) * 0.3;
    
    // SECONDARY WAVES - cross patterns
    waves += sin(pos.x * 0.25 + pos.y * 0.15 + time * 0.07) * 0.25;
    waves += sin(pos.x * 0.18 - pos.y * 0.22 - time * 0.055) * 0.2;
    
    return waves;
}

// Medium undulations
float mediumWaves(vec2 pos, float time) {
    float waves = 0.0;
    
    waves += sin(pos.x * 0.4 + time * 0.12) * 0.15;
    waves += sin(pos.y * 0.35 - time * 0.10) * 0.12;
    waves += sin((pos.x * 0.5 + pos.y * 0.3) + time * 0.09) * 0.1;
    waves += sin((pos.x * 0.3 - pos.y * 0.5) - time * 0.08) * 0.08;
    
    return waves;
}

// Visible ripples on surface
float surfaceRipples(vec2 pos, float time) {
    float ripples = 0.0;
    
    // CONCENTRIC RIPPLES - expanding circles
    for(int i = 0; i < 8; i++) {
        float fi = float(i);
        vec2 center = vec2(
            sin(fi * 1.3 + time * 0.03) * 10.0,
            cos(fi * 1.7 + time * 0.025) * 10.0
        );
        
        float dist = length(pos - center);
        
        // Expanding ring ripples
        float ripple = sin(dist * 0.8 - time * 0.5 + fi);
        ripple *= exp(-dist * 0.04); // fade with distance
        ripple *= 0.12;
        
        ripples += ripple;
    }
    
    // RAIN-DROP STYLE RIPPLES
    for(int i = 0; i < 6; i++) {
        float fi = float(i);
        vec2 dropPos = vec2(
            sin(fi * 2.7 + time * 0.1) * 6.0,
            cos(fi * 3.2 - time * 0.08) * 6.0
        );
        
        float dist = length(pos - dropPos);
        float drop = sin(dist * 1.5 - time * 0.8);
        drop *= exp(-dist * 0.1);
        drop *= 0.08;
        
        ripples += drop;
    }
    
    // FINE TEXTURE RIPPLES
    ripples += sin(pos.x * 1.2 + time * 0.15) * 0.04;
    ripples += sin(pos.y * 1.0 - time * 0.12) * 0.04;
    ripples += sin((pos.x + pos.y) * 0.8 + time * 0.1) * 0.03;
    ripples += sin((pos.x - pos.y) * 0.9 - time * 0.09) * 0.03;
    
    return ripples;
}

// Bubble disturbances
float bubbleDisturbance(vec2 pos, float time) {
    float bubbles = 0.0;
    
    // Random bubbling spots
    for(int i = 0; i < 5; i++) {
        float fi = float(i);
        vec2 bubblePos = vec2(
            sin(fi * 3.1 + time * 0.05) * 8.0 + fi * 2.0,
            cos(fi * 2.4 + time * 0.04) * 8.0
        );
        
        float dist = length(pos - bubblePos);
        float bubble = exp(-dist * 0.3);
        
        // Pulsing bubble
        bubble *= 0.5 + 0.5 * sin(time * 2.0 + fi * 1.5);
        bubble *= 0.15;
        
        bubbles += bubble;
    }
    
    // Noise-based small bubbles
    float noiseBubble = noise2D(pos * 0.5 + time * 0.2);
    noiseBubble = pow(noiseBubble, 4.0) * 0.2;
    bubbles += noiseBubble;
    
    return bubbles;
}

// MAIN WAVE HEIGHT FUNCTION - combines everything
float getAcidWaveHeight(vec2 pos, float time) {
    float height = 0.0;
    
    // === BIG WAVES (main up/down motion) ===
    height += bigWaves(pos, time) * 1.0;
    
    // === MEDIUM WAVES ===
    height += mediumWaves(pos, time) * 0.8;
    
    // === SURFACE RIPPLES ===
    height += surfaceRipples(pos, time) * 1.0;
    
    // === BUBBLE DISTURBANCES ===
    height += bubbleDisturbance(pos, time) * 0.5;
    
    // === NOISE VARIATION ===
    float noiseVar = fbm2D(pos * 0.1 + time * 0.01);
    height += (noiseVar - 0.5) * 0.3;
    
    return height;
}

// Raymarch with better precision for waves
float raymarchAcid(vec3 camera, vec3 start, vec3 end, float depth) {
    vec3 pos = start;
    vec3 dir = normalize(end - start);
    float t = 0.0;
    float maxT = length(end - start);
    
    for(int i = 0; i < 80; i++) {
        vec3 p = start + dir * t;
        
        // Get wave height at this position
        float waveHeight = getAcidWaveHeight(p.xz, iTime);
        
        // Surface is at y = waveHeight (oscillates around 0)
        float surfaceY = waveHeight * 0.5; // scale the waves
        
        float diff = p.y - surfaceY;
        
        if(diff < 0.01) {
            return distance(camera, p);
        }
        
        // Step based on distance to surface
        t += max(0.02, diff * 0.4);
        
        if(t > maxT) break;
    }
    
    return distance(camera, start);
}

// Calculate normal showing wave detail
vec3 acidNormal(vec2 pos, float e) {
    float H = getAcidWaveHeight(pos, iTime) * 0.5;
    float Hx = getAcidWaveHeight(pos + vec2(e, 0.0), iTime) * 0.5;
    float Hz = getAcidWaveHeight(pos + vec2(0.0, e), iTime) * 0.5;
    
    vec3 N = normalize(vec3(H - Hx, e * 2.0, H - Hz));
    return N;
}

// ============ ATMOSPHERE ============

vec3 venusAtmosphere(vec3 rayDir, vec3 origin, vec2 screenUV) {
    float time = iTime;
    
    vec3 skyDark = vec3(0.18, 0.12, 0.05);
    vec3 skyMid = vec3(0.35, 0.25, 0.10);
    vec3 horizonGlow = vec3(0.55, 0.40, 0.15);
    
    float heightGrad = rayDir.y * 0.5 + 0.5;
    vec3 baseColor = mix(skyMid, skyDark, heightGrad);
    
    float horizonFactor = pow(1.0 - abs(rayDir.y), 4.0);
    baseColor = mix(baseColor, horizonGlow, horizonFactor * 0.5);
    
    vec3 clouds = renderClouds(rayDir, time);
    baseColor = mix(baseColor, clouds, 0.85);
    
    vec3 smoke = renderSmoke(rayDir, screenUV, time);
    baseColor += smoke * 0.7;
    
    float haze = fbm3D(vec3(rayDir.xz * 2.0, time * 0.01));
    haze *= horizonFactor;
    baseColor += vec3(0.35, 0.25, 0.12) * haze * 0.3;
    
    baseColor *= 0.6 + rayDir.y * 0.1;
    
    float grain = hash2(screenUV * 1000.0 + time) * 0.02;
    baseColor += grain;
    
    return baseColor;
}

// ============ ACID OCEAN RENDERING ============

vec3 renderAcidOcean(vec3 N, vec3 ray, vec3 hitPos, float dist, vec3 origin, vec2 screenUV) {
    vec3 acidDeep = vec3(0.06, 0.05, 0.01);
    vec3 acidSurface = vec3(0.35, 0.28, 0.08);
    vec3 acidBright = vec3(0.55, 0.45, 0.15);
    vec3 acidGreen = vec3(0.28, 0.30, 0.10);
    vec3 acidFoam = vec3(0.60, 0.50, 0.25);
    
    float fresnel = 0.1 + 0.9 * pow(1.0 - max(0.0, dot(-N, ray)), 4.0);
    
    vec3 R = reflect(ray, N);
    R.y = abs(R.y) * 0.3 + 0.1;
    vec3 reflection = venusAtmosphere(R, origin, screenUV) * 0.5;
    
    float depthFactor = 1.0 - exp(-dist * 0.08);
    vec3 acidBase = mix(acidSurface, acidDeep, depthFactor);
    
    // Get wave height for coloring
    float waveHeight = getAcidWaveHeight(hitPos.xz, iTime);
    
    // Brighter on wave peaks, darker in troughs
    float peakFactor = smoothstep(-0.3, 0.5, waveHeight);
    acidBase = mix(acidBase * 0.7, acidBright, peakFactor * 0.4);
    
    // Foam on wave crests
    float foam = smoothstep(0.4, 0.8, waveHeight);
    foam *= noise2D(hitPos.xz * 3.0 + iTime * 0.1);
    acidBase = mix(acidBase, acidFoam, foam * 0.5);
    
    // Oily swirls
    float swirl1 = fbm2D(hitPos.xz * 1.5 + iTime * 0.008);
    float swirl2 = fbm2D(hitPos.xz * 3.0 - iTime * 0.006);
    float oily = swirl1 * 0.6 + swirl2 * 0.4;
    
    vec3 acidColor = mix(acidBase, acidGreen, oily * 0.3);
    
    // Bubbling highlights
    float bubbles = noise2D(hitPos.xz * 6.0 + iTime * 0.4);
    bubbles = pow(bubbles, 6.0);
    acidColor += acidBright * bubbles * 0.5;
    
    // Caustics
    float caustic = fbm2D(hitPos.xz * 4.0 + iTime * 0.02);
    caustic = pow(caustic, 2.5);
    acidColor += acidBright * caustic * 0.15 * (1.0 - depthFactor);
    
    // Ripple highlights - bright spots where ripples peak
    float rippleHighlight = surfaceRipples(hitPos.xz, iTime);
    rippleHighlight = smoothstep(0.05, 0.15, rippleHighlight);
    acidColor += acidFoam * rippleHighlight * 0.3;
    
    vec3 finalColor = mix(acidColor, reflection, fresnel * 0.3);
    
    return finalColor;
}

vec3 acidVaporAndSmoke(vec3 ray, vec3 hitPos, float dist, vec2 screenUV) {
    vec3 vaporColor = vec3(0.40, 0.30, 0.14);
    vec3 smokeColor = vec3(0.28, 0.20, 0.10);
    
    float vapor = 0.0;
    
    vec3 vaporPos = vec3(hitPos.xz * 0.3, iTime * 0.025);
    vaporPos.y -= iTime * 0.04;
    vapor += turbulence3D(vaporPos) * 0.5;
    
    for(int i = 0; i < 4; i++) {
        float fi = float(i);
        vec2 colPos = vec2(sin(fi * 2.1) * 5.0, cos(fi * 1.6) * 5.0);
        float colDist = length(hitPos.xz - colPos);
        
        vec3 colVapor = vec3(hitPos.xz * 0.4, iTime * 0.03 + fi);
        float col = exp(-colDist * 0.15) * turbulence3D(colVapor);
        vapor += col * 0.3;
    }
    
    float horizonBoost = pow(1.0 - abs(ray.y), 3.0);
    vapor *= 1.0 + horizonBoost * 2.5;
    vapor *= exp(-dist * 0.015) + 0.3;
    
    vec3 surfSmokePos = vec3(hitPos.xz * 0.25, iTime * 0.015);
    float surfSmoke = turbulence3D(surfSmokePos);
    surfSmoke *= horizonBoost * 0.8;
    
    return vaporColor * vapor * 0.5 + smokeColor * surfSmoke * 0.4;
}

// ============ UTILITY FUNCTIONS ============

mat3 rotationMatrix(vec3 axis, float angle) {
    float s = sin(angle);
    float c = cos(angle);
    float oc = 1.0 - c;
    return mat3(
        oc * axis.x * axis.x + c, oc * axis.x * axis.y - axis.z * s, oc * axis.z * axis.x + axis.y * s,
        oc * axis.x * axis.y + axis.z * s, oc * axis.y * axis.y + c, oc * axis.y * axis.z - axis.x * s,
        oc * axis.z * axis.x - axis.y * s, oc * axis.y * axis.z + axis.x * s, oc * axis.z * axis.z + c
    );
}

vec3 getRay(vec2 fragCoord) {
    vec2 uv = ((fragCoord.xy / iResolution.xy) * 2.0 - 1.0) * vec2(iResolution.x / iResolution.y, 1.0);
    vec3 proj = normalize(vec3(uv.x, uv.y, 1.5));
    
    if(iResolution.x < 600.0) return proj;
    
    float mx = NormalizedMouse.x;
    float my = NormalizedMouse.y;
    if(mx == 0.0 && my == 0.0) my = 0.4;
    
    mat3 rotY = rotationMatrix(vec3(0.0, -1.0, 0.0), (mx * 2.0 - 1.0) * 3.0);
    mat3 rotX = rotationMatrix(vec3(1.0, 0.0, 0.0), (my * 2.0 - 1.0) * 1.5 + 0.5);
    
    return rotY * rotX * proj;
}

float intersectPlane(vec3 origin, vec3 direction, vec3 point, vec3 normal) {
    return clamp(dot(point - origin, normal) / dot(direction, normal), -1.0, 9991999.0);
}

vec3 acesTonemap(vec3 color) {
    mat3 m1 = mat3(
        0.59719, 0.07600, 0.02840,
        0.35458, 0.90834, 0.13383,
        0.04823, 0.01566, 0.83777
    );
    mat3 m2 = mat3(
        1.60475, -0.10208, -0.00327,
        -0.53108, 1.10813, -0.07276,
        -0.07367, -0.00605, 1.07602
    );
    vec3 v = m1 * color;
    vec3 a = v * (v + 0.0245786) - 0.000090537;
    vec3 b = v * (0.983729 * v + 0.4329510) + 0.238081;
    return pow(clamp(m2 * (a / b), 0.0, 1.0), vec3(1.0 / 2.2));
}

// ============ MAIN ============

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 screenUV = fragCoord.xy / iResolution.xy;
    vec3 ray = getRay(fragCoord);
    vec3 origin = vec3(iTime * 0.02, CAMERA_HEIGHT, iTime * 0.01);
    
    // === RENDER SKY ===
    if(ray.y >= 0.0) {
        vec3 C = venusAtmosphere(ray, origin, screenUV);
        
        vec3 extraSmoke = vec3(0.25, 0.18, 0.08);
        float smokeOverlay = turbulence3D(vec3(screenUV * 2.0, iTime * 0.02));
        smokeOverlay += fbm2D(screenUV * 4.0 + iTime * 0.01) * 0.5;
        C += extraSmoke * smokeOverlay * 0.25;
        
        fragColor = vec4(acesTonemap(C * 1.5), 1.0);
        return;
    }
    
    // === RENDER ACID OCEAN ===
    // Use wider plane bounds for wave height
    vec3 planeHigh = vec3(0.0, 1.0, 0.0);  // Higher to catch wave peaks
    vec3 planeLow = vec3(0.0, -1.5, 0.0);   // Lower for wave troughs
    
    float hitHigh = intersectPlane(origin, ray, planeHigh, vec3(0.0, 1.0, 0.0));
    float hitLow = intersectPlane(origin, ray, planeLow, vec3(0.0, 1.0, 0.0));
    
    vec3 highPos = origin + ray * hitHigh;
    vec3 lowPos = origin + ray * hitLow;
    
    float dist = raymarchAcid(origin, highPos, lowPos, ACID_DEPTH);
    vec3 hitPos = origin + ray * dist;
    
    // Normal with minimal smoothing to show all wave detail
    vec3 N = acidNormal(hitPos.xz, 0.01);
    
    // Very light smoothing only at far distances
    float smoothFactor = 0.1 * min(1.0, dist * 0.005);
    N = mix(N, vec3(0.0, 1.0, 0.0), smoothFactor);
    
    // Render acid
    vec3 acidColor = renderAcidOcean(N, ray, hitPos, dist, origin, screenUV);
    
    // Add vapor and smoke
    vec3 vaporSmoke = acidVaporAndSmoke(ray, hitPos, dist, screenUV);
    acidColor += vaporSmoke;
    
    // Distance fog
    float fogAmount = 1.0 - exp(-dist * 0.008);
    vec3 fogColor = vec3(0.30, 0.20, 0.08);
    acidColor = mix(acidColor, fogColor, fogAmount * 0.6);
    
    // Surface smog
    float surfaceSmog = pow(1.0 - abs(ray.y), 4.0);
    vec3 smogColor = vec3(0.35, 0.25, 0.10);
    float smogDensity = fbm2D(hitPos.xz * 0.15 + iTime * 0.003);
    smogDensity += turbulence3D(vec3(hitPos.xz * 0.2, iTime * 0.01)) * 0.5;
    acidColor = mix(acidColor, smogColor, surfaceSmog * smogDensity * 0.4);
    
    // Screen smoke
    float screenSmoke = turbulence3D(vec3(screenUV * 3.0, iTime * 0.015));
    acidColor += vec3(0.18, 0.13, 0.05) * screenSmoke * 0.12;
    
    fragColor = vec4(acesTonemap(acidColor * 1.4), 1.0);
}
