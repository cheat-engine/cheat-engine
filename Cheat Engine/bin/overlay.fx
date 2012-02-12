//credits: ms d3d tutorials which I hacked apart

Texture2D txDiffuse : register( t0 );
SamplerState samLinear : register( s0 );

cbuffer ConstantBuffer : register( b0 )
{	
	float2 translation;
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
    r.Pos[0]+=translation[0];
    r.Pos[1]+=translation[1];
   
    return r;
}


//--------------------------------------------------------------------------------------
// Pixel Shader
//--------------------------------------------------------------------------------------



float4 PS( PS_INPUT input): SV_Target
{
    float4 r;
    r=txDiffuse.Sample( samLinear, input.Tex );      
    

    if ((r[0]+r[1]+r[2]==3) && (r[3]==1))
      r[3]=0.0f; //pure white with no transparency. Set it to see through
    else
      r[3]=r[3]*transparency;

    return r;

}

float4 PSNormal( PS_INPUT input): SV_Target
{
    //pixel shader for overlays that do not use the 255,255,255 = transparency rule
    float4 r;
    r=txDiffuse.Sample( samLinear, input.Tex )*transparency;      
    r[3]=r[3]*transparency;

    return r; 
}
