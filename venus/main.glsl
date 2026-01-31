// Venus Sulfuric Wasteland Shader
// Heavy atmosphere, acid ocean, thick clouds and smog
// https://github.com/SachinSSh

#define DRAG_MULT 0.05 
#define ACID_DEPTH 0.25 
#define CAMERA_HEIGHT 1.2
#define ITERATIONS_RAYMARCH 10
#define ITERATIONS_NORMAL 16

#define NormalizedMouse (iMouse.xy / iResolution.xy)

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

vec2 acidwavedx(vec2 position, vec2 direction, float frequency, float timeshift) {
    float x = dot(direction, position) * frequency + timeshift;
    float wave = exp(sin(x) - 1.0) * 0.15; 
    float dx = wave * cos(x) * 0.15;
    return vec2(wave, -dx);
}

float getripples(vec2 position, float time) {
    float ripple = 0.0;
    
    for(int i = 0; i < 5; i++) {
        float freq = 8.0 + float(i) * 5.0;
        float amp = 0.003 / (float(i) + 1.0);
        float phase = noise(position * 0.1 + float(i)) * 6.28;
        ripple += sin(position.x * freq + time * 0.02 + phase) * amp;
        ripple += sin(position.y * freq * 0.8 - time * 0.015 + phase * 0.5) * amp;
    }
    float bubble = noise(position * 3.0 + time * 0.1);
    bubble = pow(bubble, 3.0) * 0.01;
    
    return ripple + bubble;
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
    
    float ripples = getripples(position, iTime) * 2.0;
    
    return mainWaves * 0.6 + ripples;
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

float cloudDensity(vec3 p) {
    float density = 0.0;
    
    vec3 p1 = p * 0.3;
    p1.x += iTime * 0.002;
    density += fbm3D(p1) * 0.6;
    
    vec3 p2 = p * 0.6;
    p2.z -= iTime * 0.003;
    density += fbm3D(p2) * 0.3;
    
    vec3 p3 = p * 1.2;
    p3.xy += iTime * 0.001;
    density += fbm3D(p3) * 0.1;
    
    return density;
}

float smogDensity(vec3 p, float horizonFactor) {
    vec3 smokePos = p;
    smokePos.y *= 0.3;
    smokePos.x += iTime * 0.005;
    smokePos.z += sin(iTime * 0.01) * 0.5;
    
    float smoke = turbulence(smokePos * 0.4);
    smoke *= horizonFactor;
    smoke *= 1.5;
    
    return smoke;
}

float risingSmoke(vec3 raydir, vec2 uv) {
    vec3 smokePos = vec3(uv * 2.0, iTime * 0.02);
    
    smokePos.y -= iTime * 0.03;
    smokePos.x += sin(smokePos.y * 2.0 + iTime * 0.1) * 0.3;
    
    float smoke = turbulence(smokePos);
    smoke = pow(smoke, 1.5);
    
    float horizonBoost = pow(1.0 - abs(raydir.y), 4.0);
    smoke *= horizonBoost;
    
    return smoke * 0.4;
}

vec3 venusAtmosphere(vec3 raydir, vec3 origin) {
    vec3 sulfurYellow = vec3(0.65, 0.50, 0.12);
    vec3 darkOrange = vec3(0.40, 0.22, 0.06);
    vec3 brownSmog = vec3(0.25, 0.15, 0.05);
    vec3 paleYellow = vec3(0.80, 0.65, 0.30);
    vec3 sicklyGreen = vec3(0.45, 0.42, 0.15);
    
    float horizonFactor = pow(1.0 - abs(raydir.y), 4.0);
    float horizonFactor2 = pow(1.0 - abs(raydir.y), 2.0);
    
    float heightGrad = raydir.y * 0.5 + 0.5;
    
    vec2 cloudUV = raydir.xz / (abs(raydir.y) + 0.02);
    
    float cloud1 = fbm(cloudUV * 0.15 + iTime * 0.001);
    float cloud2 = fbm(cloudUV * 0.3 - iTime * 0.0015);
    float cloud3 = fbm(cloudUV * 0.6 + vec2(iTime * 0.002, -iTime * 0.001));
    float cloud4 = fbm(cloudUV * 1.2 - iTime * 0.001);
    
    float cloudDens = cloud1 * 0.4 + cloud2 * 0.3 + cloud3 * 0.2 + cloud4 * 0.1;
    
    float thickClouds = smoothstep(0.25, 0.7, cloudDens);
    float mediumClouds = smoothstep(0.15, 0.5, cloudDens);
    
    vec2 smogUV = cloudUV * 0.1;
    float smog1 = fbm(smogUV + iTime * 0.0005);
    float smog2 = fbm(smogUV * 2.0 - iTime * 0.0003);
    float smogLayer = (smog1 * 0.6 + smog2 * 0.4);
    smogLayer = pow(smogLayer, 0.7);
    smogLayer *= horizonFactor * 1.5 + 0.3; // Always some smog, more at horizon
    
    vec3 smokePos = vec3(cloudUV * 0.5, iTime * 0.01);
    float smoke = turbulence(smokePos * 0.8);
    smoke *= horizonFactor2 * 0.6;
    
    // ============ COLOR COMPOSITION ============
    
    // Base color - very dark, oppressive
    vec3 baseColor = mix(brownSmog, darkOrange, heightGrad * 0.5);
    baseColor *= 0.4; // Keep it dark
    
    // Dim diffuse glow from hidden sun
    float diffuseGlow = 0.15 + max(0.0, raydir.y * 0.1);
    
    // Cloud colors - varying yellows/browns
    vec3 cloudColor1 = mix(darkOrange, sulfurYellow, thickClouds);
    vec3 cloudColor2 = mix(brownSmog, paleYellow, cloud2);
    vec3 cloudColorFinal = mix(cloudColor1, cloudColor2, 0.3);
    
    // Smog color
    vec3 smogColor = mix(brownSmog, sicklyGreen, smog1 * 0.3);
    
    // Horizon glow - dim, diffused light
    vec3 horizonGlow = sulfurYellow * horizonFactor * 0.25;
    
    // ============ COMBINE EVERYTHING ============
    
    vec3 atmosphere = baseColor;
    
    // Add cloud layers
    atmosphere = mix(atmosphere, cloudColorFinal * 0.5, thickClouds * 0.8);
    atmosphere = mix(atmosphere, cloudColor2 * 0.4, mediumClouds * 0.5);
    
    // Add thick smog
    atmosphere = mix(atmosphere, smogColor * 0.3, smogLayer * 0.7);
    
    // Add smoke
    atmosphere += brownSmog * smoke * 0.3;
    
    // Add horizon glow
    atmosphere += horizonGlow;
    
    // Overall dimness - Venus surface is dark
    atmosphere *= diffuseGlow + 0.2;
    
    // Color variation for realism
    float colorVar = noise(cloudUV * 5.0 + iTime * 0.01);
    atmosphere = mix(atmosphere, atmosphere * vec3(1.0, 0.95, 0.85), colorVar * 0.2);
    
    // Grain for thick atmosphere feel
    float grain = hash(raydir.xz * 1000.0 + iTime) * 0.03;
    atmosphere += grain;
    
    return atmosphere;
}

// ============ ACID OCEAN ============

vec3 getAcidColor(vec3 N, vec3 ray, vec3 hitPos, float dist, vec3 origin) {
    // Sulfuric acid colors - sickly yellow-green, murky
    vec3 deepAcid = vec3(0.08, 0.06, 0.01);
    vec3 surfaceAcid = vec3(0.35, 0.30, 0.08);
    vec3 acidHighlight = vec3(0.55, 0.45, 0.15);
    vec3 acidGreen = vec3(0.30, 0.32, 0.10);
    
    // Fresnel - acid has higher refractive index
    float fresnel = 0.15 + 0.85 * pow(1.0 - max(0.0, dot(-N, ray)), 3.5);
    
    // Reflection of murky atmosphere
    vec3 R = normalize(reflect(ray, N));
    R.y = abs(R.y) * 0.5 + 0.1; // Keep reflections dim
    vec3 reflection = venusAtmosphere(R, origin) * 0.4;
    
    // Depth-based coloring
    float depthFactor = 1.0 - exp(-dist * 0.15);
    vec3 acidBase = mix(surfaceAcid, deepAcid, depthFactor);
    
    // Oily swirls on surface
    float swirl1 = fbm(hitPos.xz * 2.0 + iTime * 0.01);
    float swirl2 = fbm(hitPos.xz * 4.0 - iTime * 0.008);
    float oilSwirl = swirl1 * 0.6 + swirl2 * 0.4;
    
    // Color variations from chemical reactions
    vec3 swirlColor = mix(acidBase, acidGreen, oilSwirl * 0.4);
    swirlColor = mix(swirlColor, acidHighlight, pow(oilSwirl, 3.0) * 0.3);
    
    // Bubbling effect - acid is reactive
    float bubble = noise(hitPos.xz * 8.0 + iTime * 0.3);
    bubble = pow(bubble, 8.0);
    swirlColor += acidHighlight * bubble * 0.5;
    
    // Caustic-like subsurface patterns
    vec2 causticUV = hitPos.xz * 5.0;
    float caustic = fbm(causticUV + iTime * 0.015);
    caustic = pow(caustic, 2.5) * 0.2;
    swirlColor += acidHighlight * caustic * (1.0 - depthFactor);
    
    // Oily sheen/iridescence
    float sheenAngle = dot(N.xz, ray.xz);
    float sheen = sin(sheenAngle * 15.0 + iTime * 0.05) * 0.5 + 0.5;
    vec3 sheenColor = mix(acidGreen, surfaceAcid, sheen);
    swirlColor = mix(swirlColor, sheenColor, 0.15 * (1.0 - depthFactor));
    
    // Combine with reflection
    vec3 finalColor = mix(swirlColor, reflection, fresnel * 0.25);
    
    return finalColor;
}

// Vapor rising from acid surface
vec3 acidVapor(vec3 ray, vec3 hitPos, float dist) {
    vec3 vaporColor = vec3(0.35, 0.28, 0.12);
    
    // Vapor density decreases with viewing distance
    float vaporStrength = exp(-dist * 0.03);
    
    // Animated vapor
    vec2 vaporUV = hitPos.xz;
    vec3 vaporPos = vec3(vaporUV, iTime * 0.02);
    vaporPos.y -= iTime * 0.03;
    vaporPos.x += sin(vaporPos.z * 3.0) * 0.2;
    
    float vapor = turbulence(vaporPos * 0.8);
    vapor *= vaporStrength * 0.3;
    
    // More vapor near horizon view
    float horizonBoost = pow(1.0 - abs(ray.y), 2.0);
    vapor *= 1.0 + horizonBoost * 2.0;
    
    return vaporColor * vapor;
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

// ACES tonemapping
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
    
    // Slow drifting camera
    vec3 origin = vec3(iTime * 0.02, CAMERA_HEIGHT, iTime * 0.01);
    
    if(ray.y >= 0.0) {
        // Render thick Venus atmosphere with clouds and smog
        vec3 C = venusAtmosphere(ray, origin);
        
        // Add extra smoke/haze layer
        float smokeLayer = risingSmoke(ray, fragCoord.xy / iResolution.xy);
        C += vec3(0.3, 0.22, 0.08) * smokeLayer;
        
        fragColor = vec4(aces_tonemap(C * 2.0), 1.0);
        return;
    }
    
    // Acid ocean planes
    vec3 acidPlaneHigh = vec3(0.0, 0.0, 0.0);
    vec3 acidPlaneLow = vec3(0.0, -ACID_DEPTH, 0.0);
    
    float highPlaneHit = intersectPlane(origin, ray, acidPlaneHigh, vec3(0.0, 1.0, 0.0));
    float lowPlaneHit = intersectPlane(origin, ray, acidPlaneLow, vec3(0.0, 1.0, 0.0));
    vec3 highHitPos = origin + ray * highPlaneHit;
    vec3 lowHitPos = origin + ray * lowPlaneHit;
    
    float dist = raymarchacid(origin, highHitPos, lowHitPos, ACID_DEPTH);
    vec3 acidHitPos = origin + ray * dist;
    
    // Smoother normals for viscous liquid
    vec3 N = acidnormal(acidHitPos.xz, 0.03, ACID_DEPTH);
    N = mix(N, vec3(0.0, 1.0, 0.0), 0.75 * min(1.0, sqrt(dist * 0.02) * 1.1));
    
    // Get acid ocean color
    vec3 acidColor = getAcidColor(N, ray, acidHitPos, dist, origin);
    
    // Add vapor rising from acid
    vec3 vapor = acidVapor(ray, acidHitPos, dist);
    acidColor += vapor;
    
    // Heavy atmospheric fog
    float fogDist = 1.0 - exp(-dist * 0.015);
    vec3 fogColor = vec3(0.30, 0.20, 0.07);
    acidColor = mix(acidColor, fogColor, fogDist * 0.8);
    
    // Extra smog near surface
    float surfaceSmog = pow(1.0 - abs(ray.y), 3.0);
    vec3 smogColor = vec3(0.35, 0.25, 0.10);
    float smogNoise = fbm(acidHitPos.xz * 0.3 + iTime * 0.005);
    acidColor = mix(acidColor, smogColor, surfaceSmog * smogNoise * 0.5);
    
    fragColor = vec4(aces_tonemap(acidColor * 1.8), 1.0);
}
