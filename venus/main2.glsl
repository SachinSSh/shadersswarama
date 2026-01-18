// Venus Sulfuric Wasteland Shader - ENHANCED
// Heavy clouds, acid ocean with ripples, thick smoke everywhere
// 2024

#define DRAG_MULT 0.05
#define ACID_DEPTH 0.25
#define CAMERA_HEIGHT 1.2
#define ITERATIONS_RAYMARCH 10
#define ITERATIONS_NORMAL 16

#define NormalizedMouse (iMouse.xy / iResolution.xy)

// ============ NOISE FUNCTIONS ============

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

float hash3(vec3 p) {
    return fract(sin(dot(p, vec3(127.1, 311.7, 74.7))) * 43758.5453);
}

float noise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    return mix(
        mix(hash(i), hash(i + vec2(1.0, 0.0)), f.x),
        mix(hash(i + vec2(0.0, 1.0)), hash(i + vec2(1.0, 1.0)), f.x),
        f.y
    );
}

float noise3D(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    
    return mix(
        mix(
            mix(hash3(i), hash3(i + vec3(1,0,0)), f.x),
            mix(hash3(i + vec3(0,1,0)), hash3(i + vec3(1,1,0)), f.x),
            f.y
        ),
        mix(
            mix(hash3(i + vec3(0,0,1)), hash3(i + vec3(1,0,1)), f.x),
            mix(hash3(i + vec3(0,1,1)), hash3(i + vec3(1,1,1)), f.x),
            f.y
        ),
        f.z
    );
}

float fbm(vec2 p) {
    float value = 0.0;
    float amp = 0.5;
    float freq = 1.0;
    for(int i = 0; i < 6; i++) {
        value += amp * noise(p * freq);
        freq *= 2.0;
        amp *= 0.5;
    }
    return value;
}

float fbm3D(vec3 p) {
    float value = 0.0;
    float amp = 0.5;
    for(int i = 0; i < 5; i++) {
        value += amp * noise3D(p);
        p *= 2.0;
        amp *= 0.5;
    }
    return value;
}

// Turbulent noise for smoke
float turbulence(vec3 p) {
    float value = 0.0;
    float amp = 0.5;
    for(int i = 0; i < 6; i++) {
        value += amp * abs(noise3D(p) * 2.0 - 1.0);
        p *= 2.0;
        amp *= 0.5;
    }
    return value;
}

// ============ ENHANCED CLOUD FUNCTIONS ============

// Billowing cloud shape
float cloudShape(vec2 uv, float time) {
    float cloud = 0.0;
    
    // Large billowing formations
    cloud += fbm(uv * 0.5 + time * 0.01) * 1.0;
    cloud += fbm(uv * 1.0 - time * 0.008) * 0.5;
    cloud += fbm(uv * 2.0 + time * 0.005) * 0.25;
    cloud += fbm(uv * 4.0 - time * 0.003) * 0.125;
    
    return cloud / 1.875;
}

// Volumetric cloud layer - ENHANCED VISIBILITY
float volumetricCloud(vec3 rayDir, float layerHeight, float thickness, float time) {
    if(rayDir.y <= 0.001) return 0.0;
    
    // Ray march through cloud layer
    float t = (layerHeight - CAMERA_HEIGHT) / rayDir.y;
    if(t < 0.0) return 0.0;
    
    vec3 cloudPos = vec3(0.0, CAMERA_HEIGHT, 0.0) + rayDir * t;
    vec2 cloudUV = cloudPos.xz * 0.02;
    
    // Multiple cloud octaves for billowing effect
    float density = 0.0;
    density += fbm(cloudUV * 1.0 + time * 0.003) * 0.4;
    density += fbm(cloudUV * 2.0 - time * 0.002) * 0.3;
    density += fbm(cloudUV * 4.0 + time * 0.001) * 0.2;
    density += noise(cloudUV * 8.0) * 0.1;
    
    // Shape the clouds - make them more defined
    density = smoothstep(0.2, 0.7, density);
    density *= thickness;
    
    // Fade with distance
    float fadeDist = length(cloudPos.xz) * 0.001;
    density *= exp(-fadeDist);
    
    return density;
}

// MASSIVE CLOUD BANK - very visible thick clouds
float massiveCloudBank(vec3 rayDir, float time) {
    float totalCloud = 0.0;
    
    // Multiple cloud layers at different heights
    for(int i = 0; i < 5; i++) {
        float height = 8.0 + float(i) * 4.0;
        float thickness = 1.2 - float(i) * 0.15;
        
        float layer = volumetricCloud(rayDir, height, thickness, time + float(i) * 10.0);
        totalCloud += layer * (1.0 - totalCloud * 0.3); // Blend layers
    }
    
    return totalCloud;
}

// Horizontal cloud bands (like Venus has)
float cloudBands(vec3 rayDir, float time) {
    float bands = 0.0;
    
    float y = rayDir.y * 0.5 + 0.5; // 0 to 1
    
    // Create horizontal striations
    for(int i = 0; i < 4; i++) {
        float bandPos = 0.2 + float(i) * 0.2;
        float bandWidth = 0.08 + fbm(vec2(rayDir.x * 2.0 + time * 0.01, float(i))) * 0.05;
        
        float band = smoothstep(bandPos - bandWidth, bandPos, y);
        band *= smoothstep(bandPos + bandWidth * 2.0, bandPos + bandWidth, y);
        
        // Add texture to bands
        float tex = fbm(vec2(rayDir.x * 5.0 + time * 0.005, rayDir.z * 5.0) + float(i));
        band *= 0.5 + tex * 0.5;
        
        bands += band * 0.4;
    }
    
    return bands;
}

// ============ ENHANCED SMOKE FUNCTIONS ============

// Thick rising smoke columns
float smokeColumns(vec3 rayDir, vec2 uv, float time) {
    float smoke = 0.0;
    
    // Multiple smoke column sources
    for(int i = 0; i < 6; i++) {
        vec2 columnPos = vec2(
            sin(float(i) * 1.7 + time * 0.01) * 2.0,
            cos(float(i) * 2.3 + time * 0.008) * 2.0
        );
        
        vec2 offset = uv * 3.0 - columnPos;
        float dist = length(offset);
        
        // Column shape - rises and spreads
        float column = exp(-dist * 1.5);
        
        // Turbulent motion
        vec3 smokePos = vec3(offset, time * 0.05 + float(i));
        column *= turbulence(smokePos * 2.0) * 1.5;
        
        // Rising motion
        float rise = sin(time * 0.1 + float(i) + dist * 2.0) * 0.5 + 0.5;
        column *= rise;
        
        smoke += column * 0.3;
    }
    
    return smoke;
}

// Ground hugging smog
float groundSmog(vec3 rayDir, float time) {
    float smog = 0.0;
    
    // More smog looking down/horizontal
    float horizonFactor = pow(1.0 - abs(rayDir.y), 2.0);
    
    // Layered smog
    vec2 smogUV = rayDir.xz / (abs(rayDir.y) + 0.1);
    
    smog += fbm(smogUV * 0.3 + time * 0.002) * 0.5;
    smog += fbm(smogUV * 0.6 - time * 0.003) * 0.3;
    smog += turbulence(vec3(smogUV * 0.2, time * 0.01)) * 0.4;
    
    smog *= horizonFactor * 1.5 + 0.2;
    
    return smog;
}

// Swirling atmospheric haze
float atmosphericHaze(vec3 rayDir, float time) {
    vec3 hazePos = rayDir * 5.0;
    hazePos.x += time * 0.02;
    hazePos.z += sin(time * 0.01) * 2.0;
    
    float haze = fbm3D(hazePos * 0.5);
    haze += turbulence(hazePos * 0.3) * 0.5;
    
    // Swirl pattern
    float angle = atan(rayDir.z, rayDir.x);
    float swirl = sin(angle * 3.0 + time * 0.05 + haze * 5.0) * 0.5 + 0.5;
    haze *= 0.7 + swirl * 0.3;
    
    return haze * 0.6;
}

// ============ ACID WAVE FUNCTIONS ============

vec2 acidwavedx(vec2 position, vec2 direction, float frequency, float timeshift) {
    float x = dot(direction, position) * frequency + timeshift;
    float wave = exp(sin(x) - 1.0) * 0.15;
    float dx = wave * cos(x) * 0.15;
    return vec2(wave, -dx);
}

// ENHANCED RIPPLES - more visible
float getripples(vec2 position, float time) {
    float ripple = 0.0;
    
    // Concentric ripples from multiple sources
    for(int i = 0; i < 8; i++) {
        vec2 center = vec2(
            sin(float(i) * 3.7 + time * 0.02) * 5.0,
            cos(float(i) * 2.9 + time * 0.015) * 5.0
        );
        
        float dist = length(position - center);
        float wave = sin(dist * 3.0 - time * 0.5 + float(i)) * 0.5 + 0.5;
        wave *= exp(-dist * 0.1); // Fade with distance
        wave *= 0.015;
        
        ripple += wave;
    }
    
    // Fine surface ripples
    for(int i = 0; i < 6; i++) {
        float freq = 6.0 + float(i) * 4.0;
        float amp = 0.008 / (float(i) + 1.0);
        float phase = noise(position * 0.1 + float(i)) * 6.28;
        
        ripple += sin(position.x * freq + time * 0.03 + phase) * amp;
        ripple += sin(position.y * freq * 0.7 - time * 0.025 + phase * 0.7) * amp;
        
        // Cross ripples
        ripple += sin((position.x + position.y) * freq * 0.5 + time * 0.02) * amp * 0.5;
    }
    
    // Random bubbling ripples
    float bubble1 = noise(position * 4.0 + time * 0.2);
    float bubble2 = noise(position * 6.0 - time * 0.15);
    ripple += pow(bubble1, 4.0) * 0.02;
    ripple += pow(bubble2, 5.0) * 0.015;
    
    return ripple;
}

// Visible wave patterns
float getVisibleWaves(vec2 position, float time) {
    float waves = 0.0;
    
    // Slow rolling waves
    waves += sin(position.x * 0.3 + time * 0.02) * 0.03;
    waves += sin(position.y * 0.25 - time * 0.015) * 0.025;
    waves += sin((position.x + position.y) * 0.2 + time * 0.018) * 0.02;
    
    // Interference patterns
    waves += sin(position.x * 0.5 + position.y * 0.3 + time * 0.01) * 0.015;
    waves += sin(position.x * 0.3 - position.y * 0.5 - time * 0.012) * 0.015;
    
    return waves;
}

float getacidwaves(vec2 position, int iterations) {
    float wavePhaseShift = length(position) * 0.02;
    float iter = 0.0;
    float frequency = 0.15;
    float timeMultiplier = 0.008;
    float weight = 1.0;
    float sumOfValues = 0.0;
    float sumOfWeights = 0.0;
    
    int actualIter = min(iterations, 4);
    
    for(int i = 0; i < actualIter; i++) {
        vec2 p = vec2(sin(iter), cos(iter));
        vec2 res = acidwavedx(position, p, frequency, iTime * timeMultiplier + wavePhaseShift);
        position += p * res.y * weight * DRAG_MULT;
        sumOfValues += res.x * weight;
        sumOfWeights += weight;
        weight = mix(weight, 0.0, 0.4);
        frequency *= 1.8;
        timeMultiplier *= 1.02;
        iter += 1232.399963;
    }
    
    float mainWaves = sumOfValues / sumOfWeights;
    
    // ADD visible waves
    float visibleWaves = getVisibleWaves(position, iTime);
    
    // ADD enhanced ripples
    float ripples = getripples(position, iTime) * 3.0;
    
    return mainWaves * 0.5 + visibleWaves + ripples;
}

float raymarchacid(vec3 camera, vec3 start, vec3 end, float depth) {
    vec3 pos = start;
    vec3 dir = normalize(end - start);
    for(int i = 0; i < 48; i++) {
        float height = getacidwaves(pos.xz, ITERATIONS_RAYMARCH) * depth - depth;
        if(height + 0.005 > pos.y) {
            return distance(pos, camera);
        }
        pos += dir * (pos.y - height) * 0.8;
    }
    return distance(start, camera);
}

vec3 acidnormal(vec2 pos, float e, float depth) {
    vec2 ex = vec2(e, 0);
    float H = getacidwaves(pos.xy, ITERATIONS_NORMAL) * depth;
    vec3 a = vec3(pos.x, H, pos.y);
    return normalize(
        cross(
            a - vec3(pos.x - e, getacidwaves(pos.xy - ex.xy, ITERATIONS_NORMAL) * depth, pos.y),
            a - vec3(pos.x, getacidwaves(pos.xy + ex.yx, ITERATIONS_NORMAL) * depth, pos.y + e)
        )
    );
}

// ============ MAIN ATMOSPHERE FUNCTION ============

vec3 venusAtmosphere(vec3 raydir, vec3 origin) {
    // Venus color palette
    vec3 sulfurYellow = vec3(0.65, 0.50, 0.12);
    vec3 darkOrange = vec3(0.40, 0.22, 0.06);
    vec3 brownSmog = vec3(0.25, 0.15, 0.05);
    vec3 paleYellow = vec3(0.80, 0.65, 0.30);
    vec3 sicklyGreen = vec3(0.45, 0.42, 0.15);
    vec3 cloudWhite = vec3(0.75, 0.60, 0.35);
    vec3 darkCloud = vec3(0.35, 0.25, 0.12);
    
    float time = iTime;
    
    // Horizon calculations
    float horizonFactor = pow(1.0 - abs(raydir.y), 4.0);
    float heightGrad = raydir.y * 0.5 + 0.5;
    
    vec2 cloudUV = raydir.xz / (abs(raydir.y) + 0.05);
    vec2 uv = raydir.xz;
    
    // ============ MASSIVE CLOUD LAYERS ============
    
    // Layer 1: Huge billowing clouds
    float bigCloud1 = cloudShape(cloudUV * 0.3, time * 0.5);
    bigCloud1 = smoothstep(0.3, 0.8, bigCloud1);
    
    // Layer 2: Medium cloud formations  
    float bigCloud2 = cloudShape(cloudUV * 0.5 + 10.0, time * 0.4);
    bigCloud2 = smoothstep(0.35, 0.75, bigCloud2);
    
    // Layer 3: Smaller cloud details
    float detailCloud = fbm(cloudUV * 2.0 + time * 0.01);
    detailCloud = smoothstep(0.4, 0.7, detailCloud);
    
    // Layer 4: Wispy high clouds
    float wispyCloud = fbm(cloudUV * 4.0 - time * 0.005);
    wispyCloud = pow(wispyCloud, 1.5) * 0.8;
    
    // Volumetric cloud bank
    float volumeClouds = massiveCloudBank(raydir, time);
    
    // Horizontal bands
    float bands = cloudBands(raydir, time);
    
    // Combine all cloud layers
    float totalClouds = 0.0;
    totalClouds += bigCloud1 * 0.9;
    totalClouds += bigCloud2 * 0.7;
    totalClouds += detailCloud * 0.4;
    totalClouds += wispyCloud * 0.3;
    totalClouds += volumeClouds * 0.8;
    totalClouds += bands * 0.5;
    totalClouds = min(totalClouds, 1.5);
    
    // ============ SMOKE AND SMOG ============
    
    // Rising smoke columns
    float smokeCol = smokeColumns(raydir, uv, time);
    
    // Ground level smog
    float smog = groundSmog(raydir, time);
    
    // Atmospheric haze
    float haze = atmosphericHaze(raydir, time);
    
    // Turbulent smoke
    vec3 smokePos = vec3(cloudUV * 0.5, time * 0.02);
    float turbSmoke = turbulence(smokePos);
    turbSmoke *= 0.6;
    
    // Total smoke/smog
    float totalSmoke = smokeCol + smog * 0.8 + haze * 0.5 + turbSmoke * 0.4;
    
    // ============ COLOR COMPOSITION ============
    
    // Base atmosphere color
    vec3 baseColor = mix(brownSmog, darkOrange, heightGrad * 0.3);
    baseColor *= 0.5;
    
    // Cloud colors - varying tones
    vec3 cloudColor1 = mix(darkCloud, cloudWhite, bigCloud1);
    vec3 cloudColor2 = mix(darkOrange, paleYellow, bigCloud2);
    vec3 cloudMix = mix(cloudColor1, cloudColor2, 0.5);
    
    // Add self-shadowing to clouds
    float cloudShadow = 1.0 - bigCloud1 * 0.3;
    cloudMix *= cloudShadow;
    
    // Smoke color
    vec3 smokeColor = mix(brownSmog, sicklyGreen, turbSmoke * 0.3);
    smokeColor = mix(smokeColor, darkOrange, smog * 0.2);
    
    // Horizon glow
    vec3 horizonGlow = sulfurYellow * horizonFactor * 0.3;
    
    // ============ FINAL COMPOSITION ============
    
    vec3 atmosphere = baseColor;
    
    // Add clouds - VERY VISIBLE
    atmosphere = mix(atmosphere, cloudMix, totalClouds * 0.85);
    
    // Add smoke on top
    atmosphere = mix(atmosphere, smokeColor * 0.5, totalSmoke * 0.6);
    
    // Add extra turbulent layer
    float extraTurb = turbulence(vec3(raydir.xz * 2.0, time * 0.01));
    atmosphere += brownSmog * extraTurb * 0.15;
    
    // Horizon glow
    atmosphere += horizonGlow;
    
    // Dim diffuse lighting (no sun visible)
    float diffuse = 0.3 + raydir.y * 0.1;
    atmosphere *= diffuse + 0.4;
    
    // Color variation
    float colorVar = noise(cloudUV * 3.0 + time * 0.005);
    atmosphere = mix(atmosphere, atmosphere * vec3(1.0, 0.92, 0.8), colorVar * 0.2);
    
    // Atmospheric grain
    float grain = hash(raydir.xz * 1000.0 + time) * 0.02;
    atmosphere += grain;
    
    return atmosphere;
}

// ============ ACID OCEAN ============

vec3 getAcidColor(vec3 N, vec3 ray, vec3 hitPos, float dist, vec3 origin) {
    vec3 deepAcid = vec3(0.08, 0.06, 0.01);
    vec3 surfaceAcid = vec3(0.35, 0.30, 0.08);
    vec3 acidHighlight = vec3(0.55, 0.45, 0.15);
    vec3 acidGreen = vec3(0.30, 0.32, 0.10);
    
    float fresnel = 0.15 + 0.85 * pow(1.0 - max(0.0, dot(-N, ray)), 3.5);
    
    vec3 R = normalize(reflect(ray, N));
    R.y = abs(R.y) * 0.5 + 0.1;
    vec3 reflection = venusAtmosphere(R, origin) * 0.4;
    
    float depthFactor = 1.0 - exp(-dist * 0.15);
    vec3 acidBase = mix(surfaceAcid, deepAcid, depthFactor);
    
    // Oily swirls
    float swirl1 = fbm(hitPos.xz * 2.0 + iTime * 0.01);
    float swirl2 = fbm(hitPos.xz * 4.0 - iTime * 0.008);
    float oilSwirl = swirl1 * 0.6 + swirl2 * 0.4;
    
    vec3 swirlColor = mix(acidBase, acidGreen, oilSwirl * 0.4);
    swirlColor = mix(swirlColor, acidHighlight, pow(oilSwirl, 3.0) * 0.3);
    
    // Bubbling
    float bubble = noise(hitPos.xz * 8.0 + iTime * 0.3);
    bubble = pow(bubble, 8.0);
    swirlColor += acidHighlight * bubble * 0.5;
    
    // Caustics
    vec2 causticUV = hitPos.xz * 5.0;
    float caustic = fbm(causticUV + iTime * 0.015);
    caustic = pow(caustic, 2.5) * 0.2;
    swirlColor += acidHighlight * caustic * (1.0 - depthFactor);
    
    // Oily sheen
    float sheenAngle = dot(N.xz, ray.xz);
    float sheen = sin(sheenAngle * 15.0 + iTime * 0.05) * 0.5 + 0.5;
    vec3 sheenColor = mix(acidGreen, surfaceAcid, sheen);
    swirlColor = mix(swirlColor, sheenColor, 0.15 * (1.0 - depthFactor));
    
    vec3 finalColor = mix(swirlColor, reflection, fresnel * 0.25);
    
    return finalColor;
}

// ENHANCED vapor from acid
vec3 acidVapor(vec3 ray, vec3 hitPos, float dist) {
    vec3 vaporColor = vec3(0.40, 0.32, 0.15);
    
    float vaporStrength = exp(-dist * 0.02);
    
    // Multiple vapor layers
    float vapor = 0.0;
    
    // Rising columns
    for(int i = 0; i < 4; i++) {
        vec2 vaporUV = hitPos.xz + vec2(float(i) * 3.0);
        vec3 vaporPos = vec3(vaporUV * 0.3, iTime * 0.03 - float(i));
        vaporPos.y -= iTime * 0.04;
        vaporPos.x += sin(vaporPos.z * 2.0 + float(i)) * 0.3;
        
        vapor += turbulence(vaporPos) * 0.25;
    }
    
    vapor *= vaporStrength;
    
    // More vapor near horizon
    float horizonBoost = pow(1.0 - abs(ray.y), 2.5);
    vapor *= 1.0 + horizonBoost * 3.0;
    
    // Swirling motion
    float swirl = sin(atan(ray.z, ray.x) * 2.0 + iTime * 0.02) * 0.5 + 0.5;
    vapor *= 0.8 + swirl * 0.4;
    
    return vaporColor * vapor * 0.8;
}

// Surface smoke layer
vec3 surfaceSmoke(vec3 ray, vec3 hitPos, float dist) {
    vec3 smokeCol = vec3(0.30, 0.22, 0.10);
    
    float smoke = 0.0;
    
    // Thick smoke hovering over acid
    vec3 smokePos = vec3(hitPos.xz * 0.4, iTime * 0.01);
    smoke += turbulence(smokePos) * 0.5;
    smoke += fbm3D(smokePos * 2.0) * 0.3;
    
    // Billowing effect
    float billow = sin(hitPos.x * 0.5 + iTime * 0.05) * sin(hitPos.z * 0.4 - iTime * 0.03);
    smoke *= 0.8 + billow * 0.3;
    
    // Fade with distance but keep some
    smoke *= exp(-dist * 0.01) + 0.2;
    
    // More at horizon
    float horizonBoost = pow(1.0 - abs(ray.y), 3.0);
    smoke *= 1.0 + horizonBoost * 2.0;
    
    return smokeCol * smoke * 0.6;
}

// ============ UTILITY FUNCTIONS ============

mat3 createRotationMatrixAxisAngle(vec3 axis, float angle) {
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
    if(iResolution.x < 600.0) {
        return proj;
    }
    return createRotationMatrixAxisAngle(vec3(0.0, -1.0, 0.0), 3.0 * ((NormalizedMouse.x + 0.5) * 2.0 - 1.0))
        * createRotationMatrixAxisAngle(vec3(1.0, 0.0, 0.0), 0.5 + 1.5 * (((NormalizedMouse.y == 0.0 ? 0.35 : NormalizedMouse.y)) * 2.0 - 1.0))
        * proj;
}

float intersectPlane(vec3 origin, vec3 direction, vec3 point, vec3 normal) {
    return clamp(dot(point - origin, normal) / dot(direction, normal), -1.0, 9991999.0);
}

vec3 aces_tonemap(vec3 color) {
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
    vec3 ray = getRay(fragCoord);
    vec3 origin = vec3(iTime * 0.015, CAMERA_HEIGHT, iTime * 0.008);
    
    if(ray.y >= 0.0) {
        // Sky with massive clouds and smoke
        vec3 C = venusAtmosphere(ray, origin);
        
        // EXTRA smoke layer on top
        vec2 screenUV = fragCoord.xy / iResolution.xy;
        float extraSmoke = turbulence(vec3(screenUV * 3.0, iTime * 0.02));
        extraSmoke += fbm(screenUV * 5.0 + iTime * 0.01) * 0.5;
        C += vec3(0.25, 0.18, 0.08) * extraSmoke * 0.2;
        
        // Smoke wisps
        float wisps = turbulence(vec3(ray.xz * 2.0, iTime * 0.03));
        wisps *= pow(1.0 - ray.y, 2.0);
        C += vec3(0.3, 0.22, 0.1) * wisps * 0.15;
        
        fragColor = vec4(aces_tonemap(C * 1.8), 1.0);
        return;
    }
    
    // Acid ocean
    vec3 acidPlaneHigh = vec3(0.0, 0.0, 0.0);
    vec3 acidPlaneLow = vec3(0.0, -ACID_DEPTH, 0.0);
    
    float highPlaneHit = intersectPlane(origin, ray, acidPlaneHigh, vec3(0.0, 1.0, 0.0));
    float lowPlaneHit = intersectPlane(origin, ray, acidPlaneLow, vec3(0.0, 1.0, 0.0));
    vec3 highHitPos = origin + ray * highPlaneHit;
    vec3 lowHitPos = origin + ray * lowPlaneHit;
    
    float dist = raymarchacid(origin, highHitPos, lowHitPos, ACID_DEPTH);
    vec3 acidHitPos = origin + ray * dist;
    
    // Less smoothing to show ripples better
    vec3 N = acidnormal(acidHitPos.xz, 0.02, ACID_DEPTH);
    N = mix(N, vec3(0.0, 1.0, 0.0), 0.5 * min(1.0, sqrt(dist * 0.02) * 1.1));
    
    vec3 acidColor = getAcidColor(N, ray, acidHitPos, dist, origin);
    
    // Add vapor
    vec3 vapor = acidVapor(ray, acidHitPos, dist);
    acidColor += vapor;
    
    // Add surface smoke
    vec3 surfSmoke = surfaceSmoke(ray, acidHitPos, dist);
    acidColor += surfSmoke;
    
    // Heavy fog
    float fogDist = 1.0 - exp(-dist * 0.012);
    vec3 fogColor = vec3(0.32, 0.22, 0.09);
    acidColor = mix(acidColor, fogColor, fogDist * 0.75);
    
    // Extra smog
    float surfaceSmog = pow(1.0 - abs(ray.y), 3.5);
    vec3 smogColor = vec3(0.38, 0.28, 0.12);
    float smogNoise = fbm(acidHitPos.xz * 0.2 + iTime * 0.003);
    smogNoise += turbulence(vec3(acidHitPos.xz * 0.3, iTime * 0.01)) * 0.5;
    acidColor = mix(acidColor, smogColor, surfaceSmog * smogNoise * 0.6);
    
    // Screen-space smoke overlay
    vec2 screenUV = fragCoord.xy / iResolution.xy;
    float screenSmoke = turbulence(vec3(screenUV * 4.0, iTime * 0.015));
    acidColor += vec3(0.2, 0.15, 0.06) * screenSmoke * 0.1;
    
    fragColor = vec4(aces_tonemap(acidColor * 1.6), 1.0);
}
