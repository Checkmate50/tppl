// https://jduncstats.com/posts/2019-03-16-kde-scratch/

/*
# define some kernels:
# gaussian kernel
kgauss(x) = 1/sqrt(2π) * exp(-1/2 * x^2)
# boxcar
kbox(x) = abs(x) <= 1 ? 1/2 : 0
# triangular
ktri(x) = abs(x) <= 1 ? 1 - abs(x) : 0

# define the KDE function
D(x, h, xi, K) =
  1/(length(xi) * h) * sum(K.((x .- xi) / h))

# evaluate KDE along the x-axis using comprehensions

dens = [D(xstep, sqrt(2.25), x, K) for xstep in x_d, K in (kgauss, kbox, ktri)]

# visualize the kernels
plot(x_d, dens, label = ["Gaussian", "Box", "Triangular"])
*/

pub fn kernel_density_estimation(x: f64, bandwith: f64, observations: Vec<f64>, k: Kernel) -> f64 {
    if bandwith <= 0.0 {
        panic!("Bandwith (h: smoothing parameter) must be positive.")
    };

    let kernel: fn(f64) -> f64 = match k {
        Kernel::Gaussian => k_gaussian,
        Kernel::Boxcar => k_boxcar,
        Kernel::Triangular => k_triangular,
        Kernel::Epanechnikov => k_epanechnikov,
    };

    // sum(kernel((x .- xi) / bandwith))

    let n: f64 = observations.len() as f64;

    // not scale-kernel!
    let scale = 1.0 / (n * bandwith);

    let weighted_distance = observations
        .into_iter()
        .map(|observation| kernel((x - observation) / bandwith))
        .sum::<f64>();

    scale * weighted_distance
}

// # define the KDE function
// D(x, h, xi, K) =
//   1/(length(xi) * h) * sum(K.((x .- xi) / h))

// x = [-2.1; -1.3; -0.4; 1.9; 5.1; 6.2];
// x_d = range(-7, 11, length = 100)

// dens = [D(xstep, sqrt(2.25), x, K) for xstep in x_d, K in (kgauss, kbox, ktri)]
//
// # visualize the kernels
// plot(x_d, dens, label = ["Gaussian", "Box", "Triangular"])

pub fn range(start: f64, end: f64, length: i32) -> Vec<f64> {
    let step: f64 = (end - start) / (length as f64);
    let mut iter = (0..length).into_iter();

    std::iter::successors(Some(start), |n| iter.next().map(|_| n + step)).collect::<Vec<f64>>()
}

pub enum Kernel {
    Gaussian,
    Boxcar,
    Triangular,
    Epanechnikov,
}

fn k_gaussian(x: f64) -> f64 {
    1.0 / f64::sqrt(2.0 * std::f64::consts::PI) * f64::exp(-1.0 / 2.0 * f64::powf(x, 2.0))
}

fn k_boxcar(x: f64) -> f64 {
    if f64::abs(x) <= 1.0 {
        1.0 / 2.0
    } else {
        0.0
    }
}

fn k_triangular(x: f64) -> f64 {
    if f64::abs(x) <= 1.0 {
        1.0 - f64::abs(x)
    } else {
        0.0
    }
}

fn k_epanechnikov(x: f64) -> f64 {
    if f64::abs(x) <= 1.0 {
        3.0 / 4.0 * (1.0 - f64::powf(x, 2.0))
    } else {
        0.0
    }
}
