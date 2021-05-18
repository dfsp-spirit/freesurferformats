# Functions for computing Euclidean distance between vertex coordinates.
# These functions are not really specific for meshes as they simple work with vertex coords (=points) without
# any knowledge or consideration of mesh topology (graph distance) or the surface (geodesic distance).

#' @title Find vertex index closest to given query coordinate using Euclidean distance.
#'
#' @param surface an fs.surface instance or a nx3 numerical matrix representing mesh points.
#'
#' @param point_coords nx3 matrix of query coords. If a vector, will be transformed \code{byrow} to such a matrix.
#'
#' @return named list with entries: 'vertex_id' integer vector, the index of the closest vertex, and 'dist': double vector, the Euclidean distance to that vertex.
#'
#' @family Euclidean distance util functions
#'
#' @export
#' @importFrom stats dist
closest.vert.to.point <- function(surface, point_coords) {

  # Check params and extract vertex coords if needed.
  if(is.fs.surface(surface)) {
    vertices = surface$vertices;
  } else {
    if(is.matrix(surface)) {
      if(ncol(surface) == 3L) {
        vertices = surface;
      } else {
        stop("Matrix in parameter 'surface' must have 3 columns.");
      }
    } else {
      stop("Parameter 'surface' must be a numerical matrix or an fs.surface instance.");
    }
  }

  if(is.vector(point_coords)) {
    point_coords = matrix(point_coords, ncol = 3L, byrow = TRUE);
  }
  if(ncol(point_coords) != 3L) {
    stop("Parameter coords must have 3 columns.");
  }

  num_query_points = nrow(point_coords);
  if(num_query_points < 1L) {
    stop("Parameter 'point_coords' must not be empty.");
  }

  results = list('vertex_id'=rep(NA, num_query_points), 'dist'=rep(NA, num_query_points));

  for(point_idx in seq(num_query_points)) {
    point = point_coords[point_idx, ];
    dists = apply(vertices, 1, euclidian.dist, point);
    results$vertex_id[point_idx] = which.min(dists);
    results$dist[point_idx] = min(dists);
  }

  return(results);
}


#' @title Compute Euclidean distance from all mesh vertices to given point.
#'
#' @param surface an fs.surface instance
#'
#' @param point_coords double vector of length 3, the xyz coords of a single point.
#'
#' @family Euclidean distance util functions
#'
#' @return double vector of distances
vertexdists.to.point <- function(surface, point_coords) {
  assert.surface(surface);
  return(apply(surface$vertices, 1, euclidian.dist, point_coords));
}


#' @title Compute Euclidean distance.
#'
#' @param x1 numerical vector, coords of first point
#'
#' @param x2 numerical vector, coords of second point
#'
#' @return the Euclidean distance between x1 and x2.
#'
#' @keywords internal
euclidian.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#' @title Compute Euclidean distance between two vertices v1 and v2.
#'
#' @param surface an fs.surface instance
#'
#' @param v1 positive integer, vertex index of v1
#'
#' @param v2 positive integer, vertex index of v2
#'
#' @return double, the Euclidean distance between v1 and v2
#'
#' @family Euclidean distance util functions
#'
#' @export
vertex.euclid.dist <- function(surface, v1, v2) {
  assert.surface(surface);
  euclidian.dist(surface$vertices[v1, ], surface$vertices[v2, ])
}


