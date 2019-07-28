-- Stolen from https://github.com/przemyslawzaworski/CPP-Programming/blob/master/OpenCL/plasma.cl

import "lib/github.com/diku-dk/lys/lys"
import "lib/github.com/athas/vector/vspace"

module vec3 = mk_vspace_3d f32
module vec2 = mk_vspace_2d f32
type float2 = vec2.vector
type float3 = vec3.vector

let lerp (a: float3) (b: float3) (w: f32) =
  (w `vec3.scale` (b vec3.- a)) vec3.+ a

let clamp 'a (max: a -> a -> a) (min: a -> a -> a)
             (x: a) (minval: a) (maxval: a) : a =
  (x `max` minval) `min` maxval

let f32clamp = clamp f32.max f32.min

let hash (p: float2) : float3 =
  let d = {x = p.x * 0.1031,
           y = p.y * 0.1030,
           z = p.x * 0.973}
  let p3 = vec3.(d - map f32.floor d)
  let k = p3.x * (p3.y+19.19) + p3.y * (p3.x+19.19) + p3.z * (p3.z+19.19)
  let p3 = {x= p3.x+k, y = p3.y+k, z = p3.z+k}
  let q = {x = (p3.x + p3.y) * p3.z,
           y = (p3.x + p3.z) * p3.y,
           z = (p3.y + p3.z) * p3.x}
  in vec3.(q - map f32.floor q)

let noise (p: float2) : float3 =
  let ip = vec2.map f32.floor p
  let u = vec2.(p - map f32.floor p)
  let u = {x = u.x*u.x*(3.0-2.0*u.x),
           y = u.y*u.y*(3.0-2.0*u.y)}
  let res = lerp (lerp (hash ip)
                       (hash {x = ip.x+1.0, y = ip.y})
                       u.x)
                 (lerp (hash {x = ip.x,
                              y = ip.y+1.0})
                       (hash {x = ip.x+1.0,
                              y = ip.y+1.0})
                       u.x)
                 u.y
  in vec3.(res * res)

let fbm (p: float2) : float3 =
  let v = {x = 0.0, y = 0.0, z = 0.0}
  let a = {x = 0.5, y = 0.5, z = 0.5}
  let step (v, p, a) =
    let v = vec3.(v + a * noise p)
    let p = {x = (0.87 * p.x -0.48 * p.y) * 2.0,
             y = (0.48 * p.x + 0.87 * p.y) * 2.0}
    let a = vec3.(a * { x = 0.5, y = 0.5, z = 0.5})
    in (v, p, a)
  let (v,_,_) = iterate 4 step (v, p, a)
  in v

let pattern (p: float2) (iTime: f32) : float3 =
  let q = fbm {x = p.x+5.2,
               y = p.y+1.3}
  let r = fbm {x = p.x+4.0*q.x-iTime*0.5,
               y = p.y+4.0*q.y+iTime*0.3}
  in fbm {x = p.x+8.0*r.x,
          y = p.y+8.0*r.z}

let gradient (uv: float2) (delta: f32) (iTime: f32) : float2 =
  let a = pattern {x = uv.x + delta, y = uv.y} iTime
  let b = pattern {x = uv.x - delta, y = uv.y} iTime
  let c = pattern {x = uv.x, y = uv.y + delta} iTime
  let d = pattern {x = uv.x, y = uv.y - delta} iTime
  in (1/delta) `vec2.scale` {x = vec3.norm a - vec3.norm b,
                             y = vec3.norm c - vec3.norm d}

let mainImage (h: i32) (w: i32) (iTime: f32) : [h][w]i32 =
  let pixel y x =
    let uv = {x = r32 x / r32 w * 3.0,
              y = r32 y / r32 h * 3.0}
    let n = vec3.normalise (let {x,y} = gradient uv (1.0/r32 h) iTime
                            in {x,y,z=100})
    let l = vec3.normalise {x=1.0, y=1.0, z=2.0}
    let s = f32clamp (-(l.z - 2.0 * n.z * vec3.dot n l)) 0.0 1.0 ** 36.0 * 2.5
    let q = vec3.map (\v -> f32clamp v 0 1) (pattern uv iTime vec3.+ {x=s, y=s, z=s})
    in (t32 (q.z * 255) << 16) | (t32 (q.y * 255) << 8) | (t32 (q.x * 255) << 0)
  in tabulate_2d h w pixel

module lys : lys with text_content = f32 = {
  type state = { width: i32
               , height: i32
               , time: f32
               }

  let init seed height width : state = {width, height,
                                        time = r32 seed / r32 i32.highest }

  let event (e: event) (s: state) =
    match e case #step td -> s with time = s.time + td
            case _ -> s

  let resize height width (s: state) = s with width = width with height = height

  let render (s: state) =
    mainImage s.height s.width s.time

  let grab_mouse = false
  type text_content = f32
  let text_format = "FPS: %f"
  let text_content fps _ = fps : text_content
  let text_colour _ = 0i32
}
