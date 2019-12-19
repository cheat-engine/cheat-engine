//credits: ms d3d tutorials which I hacked apart

Texture2D txDiffuse : register( t0 );
SamplerState samLinear : register( s0 );

cbuffer ConstantBuffer : register( b0 )
{	
	float4x4 rotation;
        float2 originpoint;
	float2 translation;
	float2 scaling;
	float transparency;	
	float garbage;	
}




//--------------------------------------------------------------------------------------
struct VS_INPUT
{
    float4 Pos : POSITION;
    float2 Tex : TEXCOORD0;
};

struct PS_INPUT
{
    float4 Pos : SV_POSITION;
    float2 Tex : TEXCOORD0;
};


//--------------------------------------------------------------------------------------
// Vertex Shader
//--------------------------------------------------------------------------------------
PS_INPUT VS( VS_INPUT input )
{

    PS_INPUT r=input;
    float4 rp;



    r.Pos[0]-=originpoint[0];
    r.Pos[1]+=originpoint[1];
    r.Pos=mul(r.Pos, rotation);

    r.Pos[0]+=originpoint[0];
    r.Pos[1]-=originpoint[1];

    //scale to the required size (calculated by the renderer)   
    r.Pos[0]=r.Pos[0]*scaling[0];
    r.Pos[1]=r.Pos[1]*scaling[1];




    //position the sprite so the origin is at the top left
    r.Pos[0]+=1.0f*scaling[0];
    r.Pos[1]-=1.0f*scaling[1];


    //now translate to the proper position (0,0=center)
    r.Pos[0]+=translation[0];
    r.Pos[1]-=translation[1];
  

    r.Pos[2]=0.0f;
   
    return r;
}


//--------------------------------------------------------------------------------------
// Pixel Shader
//--------------------------------------------------------------------------------------


//For those wondering: Here used to be a secondary pixelshader. It's gone (yup, it's gone)

float4 PSNormal( PS_INPUT input): SV_Target
{
    //pixel shader for overlays that do not use the 255,255,255 = transparency rule
    float4 r;
    r=txDiffuse.Sample( samLinear, input.Tex ); 
    r[3]=r[3]*transparency;
    return r;

//    r[3]=r[3]*transparency;
//    return float4(0.0f, 1.0f, 0.0f, 1.0f);
}
