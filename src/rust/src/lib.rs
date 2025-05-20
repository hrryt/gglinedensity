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

fn run_series(x: &[f64], y: &[f64], width: u32, height: u32) -> (Vec<f32>, Vec<f32>) {
  let mut count = Image::new(width, height);
  for i in 0..x.len() - 1 {
    if x[i].is_nan() || x[i+1].is_nan() || y[i].is_nan() || y[i+1].is_nan() {
      continue;
    }
    draw_line_segment_mut(
      &mut count,
      (x[i] as f32, y[i] as f32),
      (x[i + 1] as f32, y[i + 1] as f32),
      Luma([1.0]),
    );
  }
  let mut density: Image = count.clone();
  for i in 0..width {
    let mut sum = 0.0;
    for j in 0..height {
      sum += count.get_pixel(i, j)[0];
    }
    if sum == 0.0 {
      continue;
    }
    for j in 0..height {
      let value = count.get_pixel(i, j)[0];
      density.put_pixel(i, j, Luma([value / sum]));
    }
  }
  (count.into_vec(), density.into_vec())
}

fn sum_images(b: Vec<f32>, mut a: Vec<f32>) -> Vec<f32> {
  for (a, b) in a.iter_mut().zip(b.iter()) {
    *a += *b;
  };
  a.to_vec()
}

#[savvy]
fn line_density(xy: ListSexp, width: i32, height: i32) -> savvy::Result<savvy::Sexp> {
  let width: u32 = width as u32;
  let height: u32 = height as u32;
  let size: usize = (width * height) as usize;
  let count_and_density: savvy::Result<(Vec<f32>, Vec<f32>)> = xy
    .values_iter()
    .map(|xyi| -> savvy::Result<(Vec<f32>, Vec<f32>)> {
      let xyi: RealSexp = match xyi.into_typed() {
        TypedSexp::Real(i) => i,
        _ => return Err(savvy_err!("xy must be list of doubles"))
      };
      let slice: &[f64] = xyi.as_slice();
      let (x, y) = slice.split_at(slice.len() / 2);
      Ok(run_series(x, y, width, height))
    })
    .fold(Ok((vec![0f32; size], vec![0f32; size])), |x, y| {
      let Ok(x) = x else {
        return x;
      };
      let Ok(y) = y else {
        return y;
      };
      Ok((sum_images(x.0, y.0), sum_images(x.1, y.1)))
    });
  let mut c_and_d = match count_and_density {
    Ok(c_and_d) => c_and_d,
    Err(e) => return Err(e)
  };
  c_and_d.0.extend(&c_and_d.1);
  let cd = c_and_d.0;
  let iter = cd.iter().map(|pixel| *pixel as f64);
  let out_sexp = OwnedRealSexp::try_from_iter(iter);
  match out_sexp {
    Ok(out) => out.into(),
    Err(e) => Err(e)
  }
}
