#version 330

in vec2 fragTexCoord;
in vec4 fragColor;
in vec3 fragPosition;
in vec3 fragNormal;

out vec4 finalColor;


uniform vec4 colDiffuse;
uniform sampler2D texture0;


void main()
{
    vec4 objectColor = texture(texture0, fragTexCoord) * colDiffuse * fragColor;
    float dotProduct = dot(fragNormal, normalize(vec3(1, 1, 0.3)));
    finalColor = vec4((0.8 + 0.2 * dotProduct) * objectColor.xyz, 1);
}
