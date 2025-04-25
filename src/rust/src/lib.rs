// // BSD-3-Clause license
//
// Copyright 2025 Dominik Moritz and contributors
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// 3. Neither the name of the copyright holder nor the names of its contributors
// may be used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// Copied and modified from <https://github.com/domoritz/line-density-rust>.

extern crate image;
extern crate imageproc;

use image::{Luma, ImageBuffer};
use imageproc::drawing::{draw_line_segment_mut};
use savvy::savvy;
use savvy::{ListSexp, TypedSexp, RealSexp, OwnedRealSexp, savvy_err};

type Image = ImageBuffer<Luma<f32>, Vec<f32>>;

fn run_series(x: &[f64], y: &[f64], width: u32, height: u32, as_is: bool) -> Vec<f32> {
    let mut data = Image::new(width, height);
    for i in 0..x.len() - 1 {
        if x[i].is_nan() || x[i+1].is_nan() || y[i].is_nan() || y[i+1].is_nan() {
          continue;
        }
        draw_line_segment_mut(
            &mut data,
            (x[i] as f32, y[i] as f32),
            (x[i + 1] as f32, y[i + 1] as f32),
            Luma([1.0]),
        );
    }
    if as_is {
      return data.into_vec();
    }
    for i in 0..width {
        let mut sum = 0.0;
        for j in 0..height {
            sum += data.get_pixel(i,j)[0];
        }
        if sum == 0.0 {
            continue;
        }
        for j in 0..height {
            let value = data.get_pixel(i,j)[0];
            data.put_pixel(i,j,Luma([value / sum]));
        }
    }
    data.into_vec()
}

fn sum_images(b: savvy::Result<Vec<f32>>, a: savvy::Result<Vec<f32>>) -> savvy::Result<Vec<f32>> {
    let Ok(mut a) = a else {
        return a;
    };
    let Ok(b) = b else {
        return b;
    };
    for (a, b) in a.iter_mut().zip(b.iter()) {
        *a += *b;
    };
    Ok(a.to_vec())
}

#[savvy]
fn line_density(xy: ListSexp, width: i32, height: i32, as_is: bool) -> savvy::Result<savvy::Sexp> {
  let width: u32 = width as u32;
  let height: u32 = height as u32;
  let pixels: savvy::Result<Vec<f32>> = xy
      .values_iter()
      .map(|xyi| -> savvy::Result<Vec<f32>> {
          let xyi: RealSexp = match xyi.into_typed() {
              TypedSexp::Real(i) => i,
              _ => return Err(savvy_err!("xy must be list of doubles"))
          };
          let slice: &[f64] = xyi.as_slice();
          let (x, y) = slice.split_at(slice.len() / 2);
          Ok(run_series(x, y, width, height, as_is))
      })
      .fold(Ok(vec![0f32; (width * height) as usize]), sum_images);
  let p = match pixels {
      Ok(p) => p,
      Err(e) => return Err(e)
  };
  let iter = p.iter().map(|pixel| *pixel as f64);
  let out_sexp = OwnedRealSexp::try_from_iter(iter);
  match out_sexp {
      Ok(out) => out.into(),
      Err(e) => Err(e)
  }
}
