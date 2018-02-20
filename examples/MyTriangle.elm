module Main exposing (main)

{-
   Rotating triangle, that is a "hello world" of the WebGL
-}

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


main : Program Never Time Time
main =
    Html.program
        { init = ( 0, Cmd.none )
        , view = view
        , subscriptions = \model -> AnimationFrame.diffs Basics.identity
        , update = \elapsed currentTime -> ( elapsed + currentTime, Cmd.none )
        }


view : Float -> Html msg
view t =
    WebGL.toHtml [ width 400, height 400 ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            uniforms
        ]



-- Mesh


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 0 0) (vec3 0 0 1)
          )
        ]



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


uniforms =
    { perspective =
        Mat4.mul
            (Mat4.makePerspective 45 1 0.01 100)
            (Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0))
    }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

      attribute vec3 position;
      attribute vec3 color;
      uniform mat4 perspective;
      varying vec3 vcolor;

      void main() {
        gl_Position = perspective * vec4(position, 1.0);
        vcolor = color;
      }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

      precision mediump float;
      varying vec3 vcolor;

      void main() {
        gl_FragColor = vec4(vcolor, 1.0);
      }

    |]
