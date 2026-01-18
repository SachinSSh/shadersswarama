// Venus Sulfuric Wasteland - MAXIMUM VISIBILITY
// Thick clouds, visible ripples, heavy smoke
// 2024

#define DRAG_MULT 0.05
#define ACID_DEPTH 0.4
#define CAMERA_HEIGHT 1.5
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

// Smooth 2D noise
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

// Smooth 3D noise
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

// FBM 2D - 6 octaves
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

// FBM 3D - 5 octaves
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

// Turbulence - for smoke
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

// ============ VERY VISIBLE CLOUDS ============

// Single cloud puff shape
float cloudPuff(vec2 uv, vec2 center, float size) {
    float d = length(uv - center);
    return smoothstep(size, size * 0.2, d);
}

// HUGE OBVIOUS CLOUDS
vec3 renderClouds(vec3 rayDir, float time) {
    // Colors
    vec3 darkCloud = vec3(0.25, 0.18, 0.08);
    vec3 midCloud = vec3(0.50, 0.38, 0.18);
    vec3 brightCloud = vec3(0.70, 0.55, 0.30);
    vec3 sulfurTint = vec3(0.65, 0.55, 0.20);
    
    float clouds = 0.0;
    float cloudDetail = 0.0;
    
    // Get UV based on ray direction
    float horizon = 1.0 - abs(rayDir.y);
    vec2 uv = rayDir.xz / (abs(rayDir.y) + 0.1);
    
    // === LAYER 1: HUGE CLOUD MASSES ===
    vec2 cloudUV1 = uv * 0.15 + time * 0.002;
    float bigCloud = fbm2D(cloudUV1);
    bigCloud = smoothstep(0.3, 0.7, bigCloud);
    clouds += bigCloud * 1.0;
    
    // === LAYER 2: MEDIUM FORMATIONS ===
    vec2 cloudUV2 = uv * 0.3 + vec2(time * 0.003, -time * 0.001);
    float medCloud = fbm2D(cloudUV2 + 50.0);
    medCloud = smoothstep(0.35, 0.65, medCloud);
    clouds += medCloud * 0.7;
    
    // === LAYER 3: BILLOWING DETAIL ===
    vec2 cloudUV3 = uv * 0.6 - time * 0.002;
    float detailCloud = fbm2D(cloudUV3 + 100.0);
    detailCloud = smoothstep(0.4, 0.6, detailCloud);
    cloudDetail = detailCloud;
    clouds += detailCloud * 0.4;
    
    // === LAYER 4: SMALL PUFFS ===
    vec2 cloudUV4 = uv * 1.2 + time * 0.001;
    float smallCloud = fbm2D(cloudUV4 + 200.0);
    smallCloud = pow(smallCloud, 1.5);
    clouds += smallCloud * 0.3;
    
    // === LAYER 5: WISPY STREAKS ===
    vec2 streakUV = uv * vec2(0.5, 2.0) + time * 0.001;
    float streaks = fbm2D(streakUV);
    streaks = pow(streaks, 2.0) * 0.5;
    clouds += streaks * 0.25;
    
    // === DRAMATIC CLOUD PUFFS ===
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
    
    // Boost clouds near horizon
    clouds *= 1.0 + horizon * 0.5;
    
    // Clamp and shape
    clouds = clamp(clouds, 0.0, 1.5);
    
    // === CLOUD COLORING ===
    vec3 cloudColor = mix(darkCloud, midCloud, clouds * 0.7);
    cloudColor = mix(cloudColor, brightCloud, pow(clouds, 2.0) * 0.5);
    cloudColor = mix(cloudColor, sulfurTint, cloudDetail * 0.3);
    
    // Add depth/shadow variation
    float shadow = fbm2D(uv * 0.4 + 30.0);
    cloudColor *= 0.7 + shadow * 0.4;
    
    return vec3(clouds, clouds, 1.0) * cloudColor;
}

// ============ MASSIVE SMOKE ============

vec3 renderSmoke(vec3 rayDir, vec2 screenUV, float time) {
    vec3 smokeColor = vec3(0.30, 0.22, 0.10);
    vec3 thickSmoke = vec3(0.20, 0.14, 0.06);
    
    float smoke = 0.0;
    
    // Get base UV
    float horizon = pow(1.0 - abs(rayDir.y), 2.0);
    vec2 uv = rayDir.xz / (abs(rayDir.y) + 0.15);
    
    // === TURBULENT SMOKE LAYER 1 ===
    vec3 smokePos1 = vec3(uv * 0.3, time * 0.02);
    smokePos1.y -= time * 0.03;
    float smoke1 = turbulence3D(smokePos1);
    smoke += smoke1 * 0.6;
    
    // === TURBULENT SMOKE LAYER 2 ===
    vec3 smokePos2 = vec3(uv * 0.5 + 10.0, time * 0.015);
    smokePos2.x += sin(time * 0.02) * 2.0;
    float smoke2 = turbulence3D(smokePos2);
    smoke += smoke2 * 0.4;
    
    // === RISING SMOKE COLUMNS ===
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
    
    // === GROUND HUGGING SMOG ===
    vec2 smogUV = uv * 0.2;
    float smog = fbm2D(smogUV + time * 0.002);
    smog += fbm2D(smogUV * 2.0 - time * 0.001) * 0.5;
    smoke += smog * horizon * 0.8;
    
    // === SCREEN SPACE SMOKE ===
    vec3 screenSmokePos = vec3(screenUV * 3.0, time * 0.01);
    float screenSmoke = turbulence3D(screenSmokePos);
    smoke += screenSmoke * 0.3;
    
    // === SWIRLING WISPS ===
    float angle = atan(rayDir.z, rayDir.x);
    float swirl = sin(angle * 3.0 + time * 0.03 + smoke * 2.0);
    smoke *= 0.9 + swirl * 0.2;
    
    // Final smoke amount
    smoke = clamp(smoke, 0.0, 1.2);
    
    // Color the smoke
    vec3 finalSmoke = mix(smokeColor, thickSmoke, smoke * 0.5);
    finalSmoke *= smoke;
    
    return finalSmoke;
}

// ============ VISIBLE ACID RIPPLES ============

// VERY visible ripples
float acidRipples(vec2 pos, float time) {
    float ripples = 0.0;
    
    // === CONCENTRIC RIPPLES FROM SOURCES ===
    for(int i = 0; i < 12; i++) {
        float fi = float(i);
        vec2 center = vec2(
            sin(fi * 1.3 + time * 0.02) * 8.0,
            cos(fi * 1.7 + time * 0.015) * 8.0
        );
        
        float dist = length(pos - center);
        float ripple = sin(dist * 2.5 - time * 0.4 + fi * 0.5);
        ripple *= exp(-dist * 0.08);
        ripple *= 0.08;
        
        ripples += ripple;
    }
    
    // === INTERFERENCE PATTERN RIPPLES ===
    ripples += sin(pos.x * 1.5 + time * 0.1) * 0.04;
    ripples += sin(pos.y * 1.2 - time * 0.08) * 0.04;
    ripples += sin((pos.x + pos.y) * 1.0 + time * 0.06) * 0.03;
    ripples += sin((pos.x - pos.y) * 0.8 - time * 0.05) * 0.03;
    
    // === FINE SURFACE RIPPLES ===
    for(int i = 0; i < 5; i++) {
        float fi = float(i);
        float freq = 3.0 + fi * 2.0;
        float amp = 0.03 / (fi + 1.0);
        float phase = noise2D(pos * 0.1 + fi) * 6.28;
        
        ripples += sin(pos.x * freq + time * 0.05 + phase) * amp;
        ripples += sin(pos.y * freq * 0.9 - time * 0.04 + phase) * amp;
    }
    
    // === RANDOM BUBBLE DISTURBANCES ===
    float bubble1 = noise2D(pos * 2.0 + time * 0.3);
    float bubble2 = noise2D(pos * 3.0 - time * 0.25);
    ripples += pow(bubble1, 3.0) * 0.1;
    ripples += pow(bubble2, 4.0) * 0.08;
    
    // === SLOW ROLLING WAVES ===
    ripples += sin(pos.x * 0.2 + time * 0.02) * 0.1;
    ripples += sin(pos.y * 0.15 - time * 0.015) * 0.08;
    
    return ripples;
}

// Main acid wave function
float getAcidWaves(vec2 position, int iterations) {
    float height = 0.0;
    
    // Base slow undulation
    height += sin(position.x * 0.1 + iTime * 0.01) * 0.15;
    height += sin(position.y * 0.08 - iTime * 0.008) * 0.12;
    height += sin((position.x + position.y) * 0.06 + iTime * 0.005) * 0.08;
    
    // Add all the ripples
    height += acidRipples(position, iTime);
    
    // Subtle FBM variation
    float fbmVar = fbm2D(position * 0.05 + iTime * 0.002);
    height += (fbmVar - 0.5) * 0.1;
    
    return height * 0.5 + 0.5;
}

// Raymarch the acid surface
float raymarchAcid(vec3 camera, vec3 start, vec3 end, float depth) {
    vec3 pos = start;
    vec3 dir = normalize(end - start);
    
    for(int i = 0; i < 64; i++) {
        float height = getAcidWaves(pos.xz, ITERATIONS_RAYMARCH) * depth - depth;
        if(height + 0.01 > pos.y) {
            return distance(pos, camera);
        }
        pos += dir * max(0.01, (pos.y - height) * 0.5);
    }
    return distance(start, camera);
}

// Calculate normal with visible ripple detail
vec3 acidNormal(vec2 pos, float e, float depth) {
    float H = getAcidWaves(pos, ITERATIONS_NORMAL) * depth;
    float Hx = getAcidWaves(pos + vec2(e, 0.0), ITERATIONS_NORMAL) * depth;
    float Hz = getAcidWaves(pos + vec2(0.0, e), ITERATIONS_NORMAL) * depth;
    
    vec3 N = normalize(vec3(H - Hx, e, H - Hz));
    return N;
}

// ============ ATMOSPHERE RENDERING ============

vec3 venusAtmosphere(vec3 rayDir, vec3 origin, vec2 screenUV) {
    float time = iTime;
    
    // Color palette
    vec3 skyDark = vec3(0.18, 0.12, 0.05);
    vec3 skyMid = vec3(0.35, 0.25, 0.10);
    vec3 horizonGlow = vec3(0.55, 0.40, 0.15);
    
    // Base gradient
    float heightGrad = rayDir.y * 0.5 + 0.5;
    vec3 baseColor = mix(skyMid, skyDark, heightGrad);
    
    // Horizon glow
    float horizonFactor = pow(1.0 - abs(rayDir.y), 4.0);
    baseColor = mix(baseColor, horizonGlow, horizonFactor * 0.5);
    
    // === ADD MASSIVE CLOUDS ===
    vec3 clouds = renderClouds(rayDir, time);
    baseColor = mix(baseColor, clouds, 0.85);
    
    // === ADD HEAVY SMOKE ===
    vec3 smoke = renderSmoke(rayDir, screenUV, time);
    baseColor += smoke * 0.7;
    
    // === EXTRA HAZE LAYER ===
    float haze = fbm3D(vec3(rayDir.xz * 2.0, time * 0.01));
    haze *= horizonFactor;
    baseColor += vec3(0.35, 0.25, 0.12) * haze * 0.3;
    
    // Dim ambient light (no sun)
    baseColor *= 0.6 + rayDir.y * 0.1;
    
    // Atmospheric grain
    float grain = hash2(screenUV * 1000.0 + time) * 0.02;
    baseColor += grain;
    
    return baseColor;
}

// ============ ACID OCEAN RENDERING ============

vec3 renderAcidOcean(vec3 N, vec3 ray, vec3 hitPos, float dist, vec3 origin, vec2 screenUV) {
    // Acid colors
    vec3 acidDeep = vec3(0.06, 0.05, 0.01);
    vec3 acidSurface = vec3(0.35, 0.28, 0.08);
    vec3 acidBright = vec3(0.55, 0.45, 0.15);
    vec3 acidGreen = vec3(0.28, 0.30, 0.10);
    
    // Fresnel
    float fresnel = 0.1 + 0.9 * pow(1.0 - max(0.0, dot(-N, ray)), 4.0);
    
    // Reflection
    vec3 R = reflect(ray, N);
    R.y = abs(R.y) * 0.3 + 0.1;
    vec3 reflection = venusAtmosphere(R, origin, screenUV) * 0.5;
    
    // Depth color
    float depthFactor = 1.0 - exp(-dist * 0.1);
    vec3 acidBase = mix(acidSurface, acidDeep, depthFactor);
    
    // Oily swirls
    float swirl1 = fbm2D(hitPos.xz * 1.5 + iTime * 0.008);
    float swirl2 = fbm2D(hitPos.xz * 3.0 - iTime * 0.006);
    float oily = swirl1 * 0.6 + swirl2 * 0.4;
    
    vec3 acidColor = mix(acidBase, acidGreen, oily * 0.4);
    acidColor = mix(acidColor, acidBright, pow(oily, 3.0) * 0.4);
    
    // Bubbling highlights
    float bubbles = noise2D(hitPos.xz * 6.0 + iTime * 0.4);
    bubbles = pow(bubbles, 6.0);
    acidColor += acidBright * bubbles * 0.6;
    
    // Caustics
    float caustic = fbm2D(hitPos.xz * 4.0 + iTime * 0.02);
    caustic = pow(caustic, 2.5);
    acidColor += acidBright * caustic * 0.2 * (1.0 - depthFactor);
    
    // Combine with reflection
    vec3 finalColor = mix(acidColor, reflection, fresnel * 0.3);
    
    return finalColor;
}

// Acid vapor and surface smoke
vec3 acidVaporAndSmoke(vec3 ray, vec3 hitPos, float dist, vec2 screenUV) {
    vec3 vaporColor = vec3(0.40, 0.30, 0.14);
    vec3 smokeColor = vec3(0.28, 0.20, 0.10);
    
    float vapor = 0.0;
    
    // Rising vapor
    vec3 vaporPos = vec3(hitPos.xz * 0.3, iTime * 0.025);
    vaporPos.y -= iTime * 0.04;
    vapor += turbulence3D(vaporPos) * 0.5;
    
    // More vapor columns
    for(int i = 0; i < 4; i++) {
        float fi = float(i);
        vec2 colPos = vec2(sin(fi * 2.1) * 5.0, cos(fi * 1.6) * 5.0);
        float colDist = length(hitPos.xz - colPos);
        
        vec3 colVapor = vec3(hitPos.xz * 0.4, iTime * 0.03 + fi);
        float col = exp(-colDist * 0.15) * turbulence3D(colVapor);
        vapor += col * 0.3;
    }
    
    // Horizon boost
    float horizonBoost = pow(1.0 - abs(ray.y), 3.0);
    vapor *= 1.0 + horizonBoost * 2.5;
    
    // Distance fade
    vapor *= exp(-dist * 0.015) + 0.3;
    
    // Surface smoke
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
        
        // Extra smoke overlay
        vec3 extraSmoke = vec3(0.25, 0.18, 0.08);
        float smokeOverlay = turbulence3D(vec3(screenUV * 2.0, iTime * 0.02));
        smokeOverlay += fbm2D(screenUV * 4.0 + iTime * 0.01) * 0.5;
        C += extraSmoke * smokeOverlay * 0.25;
        
        fragColor = vec4(acesTonemap(C * 1.5), 1.0);
        return;
    }
    
    // === RENDER ACID OCEAN ===
    vec3 planeHigh = vec3(0.0, 0.0, 0.0);
    vec3 planeLow = vec3(0.0, -ACID_DEPTH, 0.0);
    
    float hitHigh = intersectPlane(origin, ray, planeHigh, vec3(0.0, 1.0, 0.0));
    float hitLow = intersectPlane(origin, ray, planeLow, vec3(0.0, 1.0, 0.0));
    
    vec3 highPos = origin + ray * hitHigh;
    vec3 lowPos = origin + ray * hitLow;
    
    float dist = raymarchAcid(origin, highPos, lowPos, ACID_DEPTH);
    vec3 hitPos = origin + ray * dist;
    
    // Normal with minimal smoothing to show ripples
    vec3 N = acidNormal(hitPos.xz, 0.015, ACID_DEPTH);
    N = mix(N, vec3(0.0, 1.0, 0.0), 0.25 * min(1.0, sqrt(dist * 0.015)));
    
    // Acid color
    vec3 acidColor = renderAcidOcean(N, ray, hitPos, dist, origin, screenUV);
    
    // Add vapor and smoke
    vec3 vaporSmoke = acidVaporAndSmoke(ray, hitPos, dist, screenUV);
    acidColor += vaporSmoke;
    
    // Distance fog
    float fogAmount = 1.0 - exp(-dist * 0.01);
    vec3 fogColor = vec3(0.30, 0.20, 0.08);
    acidColor = mix(acidColor, fogColor, fogAmount * 0.7);
    
    // Heavy surface smog
    float surfaceSmog = pow(1.0 - abs(ray.y), 4.0);
    vec3 smogColor = vec3(0.35, 0.25, 0.10);
    float smogDensity = fbm2D(hitPos.xz * 0.15 + iTime * 0.003);
    smogDensity += turbulence3D(vec3(hitPos.xz * 0.2, iTime * 0.01)) * 0.5;
    acidColor = mix(acidColor, smogColor, surfaceSmog * smogDensity * 0.5);
    
    // Screen smoke overlay
    float screenSmoke = turbulence3D(vec3(screenUV * 3.0, iTime * 0.015));
    acidColor += vec3(0.18, 0.13, 0.05) * screenSmoke * 0.15;
    
    fragColor = vec4(acesTonemap(acidColor * 1.4), 1.0);
}
