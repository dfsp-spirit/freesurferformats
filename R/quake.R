

# md2f = '~/data/quake/barrel.md2';
# md2 = read.quake.md2(md2f);
# sf = list('faces'=(md2$triangles$vertex + 1L), vertices=md2$frames[[1]]$vertex_coords)
# class(sf) = c(class(sf), 'fs.surface')
# fsbrain::vis.fs.surface(sf)
#
# # visualiyze with texture.
# mt = rgl::material3d()
# mt$texture = '~/data/quake/barrel.png'
# mt$specular = '#000000'
# texcoords = cbind(md2$texcoords$s, md2$texcoords$t)
# tm = tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = F, material = mt, texcoords = texcoords)
# rgl::shade3d(tm)
#
# texc_t = 1.0 - (md2$texcoords$t/max(md2$texcoords$t))
# texc_s = md2$texcoords$s/max(md2$texcoords$s)
# texcoords = cbind(texc_s, texc_t)
# tm = tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = F, material = mt, texcoords = texcoords)
# rgl::shade3d(tm)
#



# seeQuake source, mdl_t struct in modelgen.h

#' @title Read Quake model in MDL format.
#'
#' @param filepath character string, the path to the MDL file
#'
#' @param fo_checks logical, whether to perform some sanity checks on the data and warn on suspicious results.
#'
#' @note Ignore this function, it will be moved to a different package.
#'
#' @examples
#' \dontrun{
#'    mdlf = "~/data/q1_pak/progs/quaddama.mdl"
#'    mdl = read.quake.mdl(mdlf);
#' }
#'
#' @export
read.quake.mdl <- function(filepath, do_checks = FALSE) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';

  mdl = list('header' = list());

  int_size = 4L;

  mdl$header$id = readBin(fh, integer(), n = 1, size = int_size, endian = endian);
  mdl$header$version = readBin(fh, integer(), n = 1, size = int_size, endian = endian);

  if(mdl$header$id != 1330660425L | mdl$header$version != 6L) {
    stop(sprintf("File '%s' not in MDL format.\n", filepath));
  }

  mdl$header$scale = readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  mdl$header$origin = readBin(fh, numeric(), n = 3, size = 4, endian = endian);
  mdl$header$radius = readBin(fh, numeric(), n = 1, size = 4, endian = endian); # bbox radius
  mdl$header$offsets = readBin(fh, numeric(), n = 3, size = 4, endian = endian); # eye pos
  mdl$header$num_skins = readBin(fh, integer(), n = 1, size = int_size, endian = endian); # number of skin textures
  mdl$header$skin_width = readBin(fh, integer(), n = 1, size = int_size, endian = endian);
  mdl$header$skin_height = readBin(fh, integer(), n = 1, size = int_size, endian = endian);
  mdl$header$num_verts = readBin(fh, integer(), n = 1, size = int_size, endian = endian);
  mdl$header$num_tris = readBin(fh, integer(), n = 1, size = int_size, endian = endian);
  mdl$header$num_frames = readBin(fh, integer(), n = 1, size = int_size, endian = endian);
  mdl$header$sync_type = readBin(fh, integer(), n = 1, size = int_size, endian = endian); # 0=synchron, 1=random
  mdl$header$flags = readBin(fh, integer(), n = 1, size = int_size, endian = endian); # 0
  mdl$header$size = readBin(fh, numeric(), n = 1, size = 4, endian = endian); # average tris size

  if(do_checks) {
    # some sanity checks
    if((mdl$header$skin_width %% 4) != 0L) {
      warning(sprintf("Invalid skin texture width %d, must be multiple of 4.\n", mdl$header$skin_width));
    }
    if((mdl$header$skin_height %% 4) != 0L) {
      warning(sprintf("Invalid skin texture height %d, must be multiple of 4.\n", mdl$header$skin_height));
    }
    if(! mdl$header$sync_type %in% c(0L, 1L)) {
      warning("Invalid sync type, must be 0 or 1.");
    }
    if(mdl$header$flags != 0L) {
      warning(sprintf("Invalid flags %d, must be 0.\n", mdl$header$flags));
    }
  }

  # next follow model skins. Could be one or a group.
  mdl$skins = list();
  mdl$skins$skin_type = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  if(mdl$skins$skin_type == 0L) { # single picture
    mdl$skins$skin_pic = readBin(fh, integer(), n = (mdl$header$skin_width * mdl$header$skin_height) , size = 1, signed = FALSE, endian = endian);
  } else {
    mdl$skins$num_skins_in_group = readBin(fh, integer(), n = 1, size = 4, endian = endian);
    mdl$skins$time_per_skin = readBin(fh, numeric(), n = mdl$skins$num_skins_in_group, size = 4, endian = endian);
    mdl$skins$skin_pics = list();
    if(mdl$skins$num_skins_in_group > 0L) {
      for(skin_idx in 1:mdl$skins$num_skins_in_group) {
        mdl$skins$skin_pics[[skin_idx]] = readBin(fh, integer(), n = (mdl$header$skin_width * mdl$header$skin_height) , size = 1, signed = FALSE, endian = endian);
      }
    }
  }

  # skin texture coords
  if(mdl$header$num_verts > 0L) {
    mdl$skins$skinverts = matrix(rep(NA, (mdl$header$num_verts * 3L)), ncol = 3L);
    for(skin_vert_idx in 1:mdl$header$num_verts) {
      # The 3 values per vertex are: onseam (whether vertex is on seam between model front and back), s (horizontal texture coord in range [0, skinwidth[), t (vertical texture coord in range [0, skinheight[).
      # The first value (onseam) must be 0 or 32L.
      mdl$skins$skinverts[skin_vert_idx,] = readBin(fh, integer(), n = 3L, size = 4, endian = endian);
    }
  }

  # triangles (as indices into vertex list)
  mdl$triangles = list();
  if(mdl$header$num_tris > 0L) {
    mdl$triangles$raw = matrix(rep(NA, (mdl$header$num_tris * 4L)), ncol = 4L);
    # the 4 values are: flag face_is_front (0=FALSE, 1s=TRUE), and the 3 vertex indices of the triangle.
    for(triangle_idx in 1:mdl$header$num_tris) {
      mdl$triangles$raw[triangle_idx,] = readBin(fh, integer(), n = 4L, size = 4, endian = endian);
    }
    mdl$triangles$triangle_is_front = mdl$triangles$raw[, 1L];
    mdl$triangles$vertex = mdl$triangles$raw[, 2:4];
    mdl$triangles$raw = NULL;
  }

  if(any(!(mdl$triangles$triangle_is_front %in% c(0L, 1L)))) {
    warning("Found triangles with invalid 'triangle_is_front' value (expected 0 or 1).");
  }
  if(max(mdl$triangles$vertex) >= mdl$header$num_verts){
    warning(sprintf("Found triangle referencing 0-based vertex index %d, but there are only %d vertices.\n", max(mdl$triangles$vertex) >= mdl$header$num_verts));
  }

  # next follow model frames. Each frame contains vertex positions (a model in a certain orientation).
  mdl$frames = list();
  if(mdl$header$num_frames > 0L) {
    for(frame_idx in 1:mdl$header$num_frames) {
      this_frame = list();
      this_frame$frame_type = readBin(fh, integer(), n = 1, size = 4, endian = endian); # 0 = simple frame, everything else = full frame.
      if(this_frame$frame_type == 0L) { # single simple frame
        # min vertex position
        this_frame$min_vertex = readBin(fh, integer(), n = 4, size = 1, signed = FALSE, endian = endian);
        # same for max vertex position.
        this_frame$max_vertex = readBin(fh, integer(), n = 4, size = 1, signed = FALSE, endian = endian);
        this_frame$name = readChar(fh, 16L); # frame name.

        # the 4 values are: 1-3=packed position 255 (x,y,z), 4=index into normal list.
        this_frame$vertex_coords_raw = matrix(readBin(fh, integer(), n = (mdl$header$num_verts * 4L), size = 1, signed = FALSE, endian = endian), ncol = 4L, byrow = TRUE);
        this_frame$vertex_coords = unpack.vertex.coords(this_frame$vertex_coords_raw[,1:3], mdl$header);
        this_frame$vertex_normals = lookup.q1.normals(this_frame$vertex_coords_raw[,4]);
        #this_frame$vertex_coords_raw = NULL;
      } else {  # full frame: group of simple frames and extra data.
        # min vertex position over all following frames. The 4 values are: 1..3=packed position in range 0..255. 4=normal index (into list of pre-defined normals, approximate value for Gouroud Shading).
        this_frame$min_vertex = readBin(fh, integer(), n = 4, size = 1, signed = FALSE, endian = endian);
        # same for max vertex position.
        this_frame$max_vertex = readBin(fh, integer(), n = 4, size = 1, signed = FALSE, endian = endian);
        this_frame$num_simple_frames = this_frame$frame_type; # TODO: where to get this? the current value this_frame$frame_type is a guess.
        this_frame$frame_timings = readBin(fh, numeric(), n = this_frame$num_simple_frames, size = 4, endian = endian);
        this_frame$simple_frames = list();
        for(simple_frame_idx in 1:this_frame$num_simple_frames) {
          this_simple_frame = list();
          this_simple_frame$min_vertex = readBin(fh, integer(), n = 4, size = 1, signed = FALSE, endian = endian);
          # same for max vertex position.
          this_simple_frame$max_vertex = readBin(fh, integer(), n = 4, size = 1, signed = FALSE, endian = endian);
          this_simple_frame$name = readChar(fh, 16L); # frame name.
          this_simple_frame$vertex_coords_raw = matrix(readBin(fh, integer(), n = (mdl$header$num_verts * 4L), size = 1, signed = FALSE, endian = endian), ncol = 4L, byrow = TRUE);
          this_simple_frame$vertex_coords = unpack.vertex.coords(this_simple_frame$vertex_coords_raw[,1:3], mdl$header);
          this_simple_frame$vertex_normals = lookup.q1.normals(this_simple_frame$vertex_coords_raw[,4]);
          this_simple_frame$vertex_coords_raw = NULL;
          this_frame$simple_frames[[simple_frame_idx]] = this_simple_frame;
        }
      }
      mdl$frames[[frame_idx]] = this_frame;
    }
  }
  class(mdl) = c(class(mdl), 'quakemodel_mdl', 'quakemodel');
  return(mdl);
}


#' @title Unpack vertex coords from Q1 0-255 representation.
#'
#' @param coords_packed matrix of n x 3 integers in range 0..255, the packed coords from an MDL file.
#'
#' @param mdl_header MDL header or named list, only the fields 'header$scale' and 'header$origin' are used.
#'
#' @keywords internal
unpack.vertex.coords <- function(coords_packed, mdl_header) {
  if(ncol(coords_packed) != 3L) {
    stop("Parameter 'coords_packed' must be a matrix with 3 columns.");
  }
  if(is.null(mdl_header$origin) | is.null(mdl_header$scale)) {
    stop("Parameter 'mdl_header' must have 'origin' and 'scale' entries.")
  }

  nc = ncol(coords_packed);
  coords_unpacked = matrix(rep(NA, (nc * nrow(coords_packed))), ncol = nc);
  for(row_idx in 1:nrow(coords_packed)) {
    coords_unpacked[row_idx,] = (coords_packed[row_idx,] * mdl_header$scale) + mdl_header$origin;
  }
  return(coords_unpacked);
}


#' @title Return list of pre-defined Quake I normals.
#'
#' @return n x 3 matrix of doubles, the normals. Hardcoded.
#'
#' @keywords internal
predefined.mdl.normals <- function() {
  q1_norms = c( -0.525731, 0.000000, 0.850651 ,
                -0.442863, 0.238856, 0.864188 ,
                -0.295242, 0.000000, 0.955423 ,
                -0.309017, 0.500000, 0.809017 ,
                -0.162460, 0.262866, 0.951056 ,
                0.000000, 0.000000, 1.000000 ,
                0.000000, 0.850651, 0.525731 ,
                -0.147621, 0.716567, 0.681718 ,
                0.147621, 0.716567, 0.681718 ,
                0.000000, 0.525731, 0.850651 ,
                0.309017, 0.500000, 0.809017 ,
                0.525731, 0.000000, 0.850651 ,
                0.295242, 0.000000, 0.955423 ,
                0.442863, 0.238856, 0.864188 ,
                0.162460, 0.262866, 0.951056 ,
                -0.681718, 0.147621, 0.716567 ,
                -0.809017, 0.309017, 0.500000 ,
                -0.587785, 0.425325, 0.688191 ,
                -0.850651, 0.525731, 0.000000 ,
                -0.864188, 0.442863, 0.238856 ,
                -0.716567, 0.681718, 0.147621 ,
                -0.688191, 0.587785, 0.425325 ,
                -0.500000, 0.809017, 0.309017 ,
                -0.238856, 0.864188, 0.442863 ,
                -0.425325, 0.688191, 0.587785 ,
                -0.716567, 0.681718, -0.147621 ,
                -0.500000, 0.809017, -0.309017 ,
                -0.525731, 0.850651, 0.000000 ,
                0.000000, 0.850651, -0.525731 ,
                -0.238856, 0.864188, -0.442863 ,
                0.000000, 0.955423, -0.295242 ,
                -0.262866, 0.951056, -0.162460 ,
                0.000000, 1.000000, 0.000000 ,
                0.000000, 0.955423, 0.295242 ,
                -0.262866, 0.951056, 0.162460 ,
                0.238856, 0.864188, 0.442863 ,
                0.262866, 0.951056, 0.162460 ,
                0.500000, 0.809017, 0.309017 ,
                0.238856, 0.864188, -0.442863 ,
                0.262866, 0.951056, -0.162460 ,
                0.500000, 0.809017, -0.309017 ,
                0.850651, 0.525731, 0.000000 ,
                0.716567, 0.681718, 0.147621 ,
                0.716567, 0.681718, -0.147621 ,
                0.525731, 0.850651, 0.000000 ,
                0.425325, 0.688191, 0.587785 ,
                0.864188, 0.442863, 0.238856 ,
                0.688191, 0.587785, 0.425325 ,
                0.809017, 0.309017, 0.500000 ,
                0.681718, 0.147621, 0.716567 ,
                0.587785, 0.425325, 0.688191 ,
                0.955423, 0.295242, 0.000000 ,
                1.000000, 0.000000, 0.000000 ,
                0.951056, 0.162460, 0.262866 ,
                0.850651, -0.525731, 0.000000 ,
                0.955423, -0.295242, 0.000000 ,
                0.864188, -0.442863, 0.238856 ,
                0.951056, -0.162460, 0.262866 ,
                0.809017, -0.309017, 0.500000 ,
                0.681718, -0.147621, 0.716567 ,
                0.850651, 0.000000, 0.525731 ,
                0.864188, 0.442863, -0.238856 ,
                0.809017, 0.309017, -0.500000 ,
                0.951056, 0.162460, -0.262866 ,
                0.525731, 0.000000, -0.850651 ,
                0.681718, 0.147621, -0.716567 ,
                0.681718, -0.147621, -0.716567 ,
                0.850651, 0.000000, -0.525731 ,
                0.809017, -0.309017, -0.500000 ,
                0.864188, -0.442863, -0.238856 ,
                0.951056, -0.162460, -0.262866 ,
                0.147621, 0.716567, -0.681718 ,
                0.309017, 0.500000, -0.809017 ,
                0.425325, 0.688191, -0.587785 ,
                0.442863, 0.238856, -0.864188 ,
                0.587785, 0.425325, -0.688191 ,
                0.688191, 0.587785, -0.425325 ,
                -0.147621, 0.716567, -0.681718 ,
                -0.309017, 0.500000, -0.809017 ,
                0.000000, 0.525731, -0.850651 ,
                -0.525731, 0.000000, -0.850651 ,
                -0.442863, 0.238856, -0.864188 ,
                -0.295242, 0.000000, -0.955423 ,
                -0.162460, 0.262866, -0.951056 ,
                0.000000, 0.000000, -1.000000 ,
                0.295242, 0.000000, -0.955423 ,
                0.162460, 0.262866, -0.951056 ,
                -0.442863, -0.238856, -0.864188 ,
                -0.309017, -0.500000, -0.809017 ,
                -0.162460, -0.262866, -0.951056 ,
                0.000000, -0.850651, -0.525731 ,
                -0.147621, -0.716567, -0.681718 ,
                0.147621, -0.716567, -0.681718 ,
                0.000000, -0.525731, -0.850651 ,
                0.309017, -0.500000, -0.809017 ,
                0.442863, -0.238856, -0.864188 ,
                0.162460, -0.262866, -0.951056 ,
                0.238856, -0.864188, -0.442863 ,
                0.500000, -0.809017, -0.309017 ,
                0.425325, -0.688191, -0.587785 ,
                0.716567, -0.681718, -0.147621 ,
                0.688191, -0.587785, -0.425325 ,
                0.587785, -0.425325, -0.688191 ,
                0.000000, -0.955423, -0.295242 ,
                0.000000, -1.000000, 0.000000 ,
                0.262866, -0.951056, -0.162460 ,
                0.000000, -0.850651, 0.525731 ,
                0.000000, -0.955423, 0.295242 ,
                0.238856, -0.864188, 0.442863 ,
                0.262866, -0.951056, 0.162460 ,
                0.500000, -0.809017, 0.309017 ,
                0.716567, -0.681718, 0.147621 ,
                0.525731, -0.850651, 0.000000 ,
                -0.238856, -0.864188, -0.442863 ,
                -0.500000, -0.809017, -0.309017 ,
                -0.262866, -0.951056, -0.162460 ,
                -0.850651, -0.525731, 0.000000 ,
                -0.716567, -0.681718, -0.147621 ,
                -0.716567, -0.681718, 0.147621 ,
                -0.525731, -0.850651, 0.000000 ,
                -0.500000, -0.809017, 0.309017 ,
                -0.238856, -0.864188, 0.442863 ,
                -0.262866, -0.951056, 0.162460 ,
                -0.864188, -0.442863, 0.238856 ,
                -0.809017, -0.309017, 0.500000 ,
                -0.688191, -0.587785, 0.425325 ,
                -0.681718, -0.147621, 0.716567 ,
                -0.442863, -0.238856, 0.864188 ,
                -0.587785, -0.425325, 0.688191 ,
                -0.309017, -0.500000, 0.809017 ,
                -0.147621, -0.716567, 0.681718 ,
                -0.425325, -0.688191, 0.587785 ,
                -0.162460, -0.262866, 0.951056 ,
                0.442863, -0.238856, 0.864188 ,
                0.162460, -0.262866, 0.951056 ,
                0.309017, -0.500000, 0.809017 ,
                0.147621, -0.716567, 0.681718 ,
                0.000000, -0.525731, 0.850651 ,
                0.425325, -0.688191, 0.587785 ,
                0.587785, -0.425325, 0.688191 ,
                0.688191, -0.587785, 0.425325 ,
                -0.955423, 0.295242, 0.000000 ,
                -0.951056, 0.162460, 0.262866 ,
                -1.000000, 0.000000, 0.000000 ,
                -0.850651, 0.000000, 0.525731 ,
                -0.955423, -0.295242, 0.000000 ,
                -0.951056, -0.162460, 0.262866 ,
                -0.864188, 0.442863, -0.238856 ,
                -0.951056, 0.162460, -0.262866 ,
                -0.809017, 0.309017, -0.500000 ,
                -0.864188, -0.442863, -0.238856 ,
                -0.951056, -0.162460, -0.262866 ,
                -0.809017, -0.309017, -0.500000 ,
                -0.681718, 0.147621, -0.716567 ,
                -0.681718, -0.147621, -0.716567 ,
                -0.850651, 0.000000, -0.525731 ,
                -0.688191, 0.587785, -0.425325 ,
                -0.587785, 0.425325, -0.688191 ,
                -0.425325, 0.688191, -0.587785 ,
                -0.425325, -0.688191, -0.587785 ,
                -0.587785, -0.425325, -0.688191 ,
                -0.688191, -0.587785, -0.425325 );
  return(matrix(q1_norms, ncol = 3L, byrow = TRUE));
}

#' @title Lookup Quake I normals by index.
#'
#' @param normal_indices integer vector of length n, the normal indices (0-based).
#'
#' @return n x 3 matrix of doubles, the normals
#'
#' @keywords internal
lookup.q1.normals <- function(normal_indices) {
  if( ! is.vector(normal_indices)) {
    stop("Parameter 'normal_indices' must be an integer vector.");
  }
  return(predefined.mdl.normals()[(normal_indices + 1L)]);
}


#' @title Read Quake II model in MD2 format.
#'
#' @param filepath character string, the path to the MD2 file
#'
#' @param anim logical, whether to load the whole animation (if present). Returns a list of models, the animation frames. If FALSE, only the first frame is returned.
#'
#' @note Ignore this function, it will be moved to a different package.
#'
#' @export
read.quake.md2 <- function(filepath, anim = FALSE) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) });

  endian = 'little';

  md2 = list();
  header = list();

  header$ident = readBin(fh, integer(), n = 1, size = 4, endian = endian);
  header$version = readBin(fh, integer(), n = 1, size = 4, endian = endian);

  if(header$ident != 844121161 | header$version != 8L) {
    stop(sprintf("File '%s' not in MD2 format.\n", filepath));
  }

  hdr_data = readBin(fh, integer(), n = 15, size = 4, endian = endian);
  header$skinwidth = hdr_data[1];
  header$skinheight = hdr_data[2];
  header$framesize = hdr_data[3];
  header$num_skins = hdr_data[4];
  header$num_vertices = hdr_data[5];
  header$num_st = hdr_data[6];
  header$num_tris = hdr_data[7];
  header$num_glcmds = hdr_data[8];
  header$num_frames = hdr_data[9];
  header$offset_skins = hdr_data[10];
  header$offset_st = hdr_data[11];
  header$offset_tris = hdr_data[12];
  header$offset_frames = hdr_data[13];
  header$offset_glcmds = hdr_data[14];
  header$offset_end = hdr_data[15];

  # read model data: skins (a.k.a. textures)
  seek(fh, where = header$offset_skins, origin = "start");
  md2$skins = list();
  if(header$num_skins > 0L) {
    for(i in 1:header$num_skins) {
      #md2$skins[[i]] = readBin(fh, character());
      md2$skins[[i]] = readChar(fh, 64L);
    }
  }

  # read model data: texture coords
  seek(fh, where = header$offset_st, origin = "start");
  md2$texcoords = list();
  md2$texcoords_unscaled = list();
  if(header$num_st > 0L) {
    md2$texcoords$s= rep(NA, (header$num_st));
    md2$texcoords$t= rep(NA, (header$num_st));
    md2$texcoords_unscaled$s= rep(NA, (header$num_st));
    md2$texcoords_unscaled$t= rep(NA, (header$num_st));
    for(i in 1:header$num_st) {
      md2$texcoords_unscaled$s[[i]] = readBin(fh, integer(), n = 1L, size = 2L);
      md2$texcoords_unscaled$t[[i]] = readBin(fh, integer(), n = 1L, size = 2L);
    }
    md2$texcoords$s = md2$texcoords_unscaled$s / header$skinwidth;
    md2$texcoords$t = md2$texcoords_unscaled$t / header$skinheight;
  }

  # read model data: triangles (vertex and texture indices)
  seek(fh, where = header$offset_tris, origin = "start");
  md2$triangles = list();
  if(header$num_tris > 0L) {
    md2$triangles$vertex = matrix(rep(NA, (header$num_tris * 3L)), ncol = 3); # vertex indices
    md2$triangles$st = matrix(rep(NA, (header$num_tris * 3L)), ncol = 3); # texture coord indices
    for(i in 1:header$num_tris) {
      md2$triangles$vertex[i,] = readBin(fh, integer(), n = 3L, size = 2L, signed = FALSE);
      md2$triangles$st[i,] = readBin(fh, integer(), n = 3L, size = 2L, signed = FALSE);
    }
  }

  if(max(md2$triangles$vertex) >= header$num_vertices){
    warning(sprintf("Found triangle referencing 0-based vertex index %d, but there are only %d vertices.\n", max(md2$triangles$vertex) >= header$num_vertices));
  }

  # read model data: openGL commands
  seek(fh, where = header$offset_glcmds, origin = "start");
  if(header$num_glcmds > 0L) {
      md2$glcmds = readBin(fh, integer(), n = header$num_glcmds, size = 4L);
  }


  ## inside frame loop:
  seek(fh, where = header$offset_frames, origin = "start");

  md2$frames = list();
  if(header$num_frames > 0L) {
    pdn = predefined.md2.normals();
    for(i in 1:header$num_frames) {
      this_frame = list();
      # read model data: vertex coords (and normal vector indices into pre-defined normal vector)
      this_frame$scale = readBin(fh, numeric(), n = 3L, size = 4L);
      this_frame$translate = readBin(fh, numeric(), n = 3L, size = 4L);
      this_frame$name = readChar(fh, 16L);
      if(header$num_vertices > 0L) {
        this_frame$vertex_coords = matrix(rep(NA, (3 * header$num_vertices)), ncol = 3L);
        this_frame$vertex_normals = matrix(rep(NA, (3 * header$num_vertices)), ncol = 3L);
        for(j in 1:header$num_vertices) {
          this_vert_coords_raw = readBin(fh, integer(), n = 3L, size = 1L, signed = FALSE);
          this_vert_normal_index = readBin(fh, integer(), n = 1L, size = 1L, signed = FALSE);

          # compute real vertex coords using frame scale and translation
          this_frame$vertex_coords[j,] = (this_frame$scale * this_vert_coords_raw) + this_frame$translate;
          this_frame$vertex_normals[j,] = pdn[this_vert_normal_index,];
        }
      }
      md2$frames[[i]] = this_frame;
    }
  }

  expected_framesize = 12L + 12L + 16L + (4L * header$num_vertices);
  if(header$framesize != expected_framesize) {
    warning(sprintf("Framesize from header is %d, expected %d.\n", header$framesize, expected_framesize));
  }

  md2$header = header;
  class(md2) = c(class(md2), 'quakemodel_md2', 'quakemodel');
  return(md2);
}


#' @title Predefined MD2 normals from Quake 2.
#'
#' @return 3xn matrix of normals.
#'
#' @keywords internal
predefined.md2.normals <- function() {
  normals_raw = c(
    -0.525731,  0.000000,  0.850651 ,
    -0.442863,  0.238856,  0.864188 ,
    -0.295242,  0.000000,  0.955423 ,
    -0.309017,  0.500000,  0.809017 ,
    -0.162460,  0.262866,  0.951056 ,
    0.000000,  0.000000,  1.000000 ,
    0.000000,  0.850651,  0.525731 ,
    -0.147621,  0.716567,  0.681718 ,
    0.147621,  0.716567,  0.681718 ,
    0.000000,  0.525731,  0.850651 ,
    0.309017,  0.500000,  0.809017 ,
    0.525731,  0.000000,  0.850651 ,
    0.295242,  0.000000,  0.955423 ,
    0.442863,  0.238856,  0.864188 ,
    0.162460,  0.262866,  0.951056 ,
    -0.681718,  0.147621,  0.716567 ,
    -0.809017,  0.309017,  0.500000 ,
    -0.587785,  0.425325,  0.688191 ,
    -0.850651,  0.525731,  0.000000 ,
    -0.864188,  0.442863,  0.238856 ,
    -0.716567,  0.681718,  0.147621 ,
    -0.688191,  0.587785,  0.425325 ,
    -0.500000,  0.809017,  0.309017 ,
    -0.238856,  0.864188,  0.442863 ,
    -0.425325,  0.688191,  0.587785 ,
    -0.716567,  0.681718, -0.147621 ,
    -0.500000,  0.809017, -0.309017 ,
    -0.525731,  0.850651,  0.000000 ,
    0.000000,  0.850651, -0.525731 ,
    -0.238856,  0.864188, -0.442863 ,
    0.000000,  0.955423, -0.295242 ,
    -0.262866,  0.951056, -0.162460 ,
    0.000000,  1.000000,  0.000000 ,
    0.000000,  0.955423,  0.295242 ,
    -0.262866,  0.951056,  0.162460 ,
    0.238856,  0.864188,  0.442863 ,
    0.262866,  0.951056,  0.162460 ,
    0.500000,  0.809017,  0.309017 ,
    0.238856,  0.864188, -0.442863 ,
    0.262866,  0.951056, -0.162460 ,
    0.500000,  0.809017, -0.309017 ,
    0.850651,  0.525731,  0.000000 ,
    0.716567,  0.681718,  0.147621 ,
    0.716567,  0.681718, -0.147621 ,
    0.525731,  0.850651,  0.000000 ,
    0.425325,  0.688191,  0.587785 ,
    0.864188,  0.442863,  0.238856 ,
    0.688191,  0.587785,  0.425325 ,
    0.809017,  0.309017,  0.500000 ,
    0.681718,  0.147621,  0.716567 ,
    0.587785,  0.425325,  0.688191 ,
    0.955423,  0.295242,  0.000000 ,
    1.000000,  0.000000,  0.000000 ,
    0.951056,  0.162460,  0.262866 ,
    0.850651, -0.525731,  0.000000 ,
    0.955423, -0.295242,  0.000000 ,
    0.864188, -0.442863,  0.238856 ,
    0.951056, -0.162460,  0.262866 ,
    0.809017, -0.309017,  0.500000 ,
    0.681718, -0.147621,  0.716567 ,
    0.850651,  0.000000,  0.525731 ,
    0.864188,  0.442863, -0.238856 ,
    0.809017,  0.309017, -0.500000 ,
    0.951056,  0.162460, -0.262866 ,
    0.525731,  0.000000, -0.850651 ,
    0.681718,  0.147621, -0.716567 ,
    0.681718, -0.147621, -0.716567 ,
    0.850651,  0.000000, -0.525731 ,
    0.809017, -0.309017, -0.500000 ,
    0.864188, -0.442863, -0.238856 ,
    0.951056, -0.162460, -0.262866 ,
    0.147621,  0.716567, -0.681718 ,
    0.309017,  0.500000, -0.809017 ,
    0.425325,  0.688191, -0.587785 ,
    0.442863,  0.238856, -0.864188 ,
    0.587785,  0.425325, -0.688191 ,
    0.688191,  0.587785, -0.425325 ,
    -0.147621,  0.716567, -0.681718 ,
    -0.309017,  0.500000, -0.809017 ,
    0.000000,  0.525731, -0.850651 ,
    -0.525731,  0.000000, -0.850651 ,
    -0.442863,  0.238856, -0.864188 ,
    -0.295242,  0.000000, -0.955423 ,
    -0.162460,  0.262866, -0.951056 ,
    0.000000,  0.000000, -1.000000 ,
    0.295242,  0.000000, -0.955423 ,
    0.162460,  0.262866, -0.951056 ,
    -0.442863, -0.238856, -0.864188 ,
    -0.309017, -0.500000, -0.809017 ,
    -0.162460, -0.262866, -0.951056 ,
    0.000000, -0.850651, -0.525731 ,
    -0.147621, -0.716567, -0.681718 ,
    0.147621, -0.716567, -0.681718 ,
    0.000000, -0.525731, -0.850651 ,
    0.309017, -0.500000, -0.809017 ,
    0.442863, -0.238856, -0.864188 ,
    0.162460, -0.262866, -0.951056 ,
    0.238856, -0.864188, -0.442863 ,
    0.500000, -0.809017, -0.309017 ,
    0.425325, -0.688191, -0.587785 ,
    0.716567, -0.681718, -0.147621 ,
    0.688191, -0.587785, -0.425325 ,
    0.587785, -0.425325, -0.688191 ,
    0.000000, -0.955423, -0.295242 ,
    0.000000, -1.000000,  0.000000 ,
    0.262866, -0.951056, -0.162460 ,
    0.000000, -0.850651,  0.525731 ,
    0.000000, -0.955423,  0.295242 ,
    0.238856, -0.864188,  0.442863 ,
    0.262866, -0.951056,  0.162460 ,
    0.500000, -0.809017,  0.309017 ,
    0.716567, -0.681718,  0.147621 ,
    0.525731, -0.850651,  0.000000 ,
    -0.238856, -0.864188, -0.442863 ,
    -0.500000, -0.809017, -0.309017 ,
    -0.262866, -0.951056, -0.162460 ,
    -0.850651, -0.525731,  0.000000 ,
    -0.716567, -0.681718, -0.147621 ,
    -0.716567, -0.681718,  0.147621 ,
    -0.525731, -0.850651,  0.000000 ,
    -0.500000, -0.809017,  0.309017 ,
    -0.238856, -0.864188,  0.442863 ,
    -0.262866, -0.951056,  0.162460 ,
    -0.864188, -0.442863,  0.238856 ,
    -0.809017, -0.309017,  0.500000 ,
    -0.688191, -0.587785,  0.425325 ,
    -0.681718, -0.147621,  0.716567 ,
    -0.442863, -0.238856,  0.864188 ,
    -0.587785, -0.425325,  0.688191 ,
    -0.309017, -0.500000,  0.809017 ,
    -0.147621, -0.716567,  0.681718 ,
    -0.425325, -0.688191,  0.587785 ,
    -0.162460, -0.262866,  0.951056 ,
    0.442863, -0.238856,  0.864188 ,
    0.162460, -0.262866,  0.951056 ,
    0.309017, -0.500000,  0.809017 ,
    0.147621, -0.716567,  0.681718 ,
    0.000000, -0.525731,  0.850651 ,
    0.425325, -0.688191,  0.587785 ,
    0.587785, -0.425325,  0.688191 ,
    0.688191, -0.587785,  0.425325 ,
    -0.955423,  0.295242,  0.000000 ,
    -0.951056,  0.162460,  0.262866 ,
    -1.000000,  0.000000,  0.000000 ,
    -0.850651,  0.000000,  0.525731 ,
    -0.955423, -0.295242,  0.000000 ,
    -0.951056, -0.162460,  0.262866 ,
    -0.864188,  0.442863, -0.238856 ,
    -0.951056,  0.162460, -0.262866 ,
    -0.809017,  0.309017, -0.500000 ,
    -0.864188, -0.442863, -0.238856 ,
    -0.951056, -0.162460, -0.262866 ,
    -0.809017, -0.309017, -0.500000 ,
    -0.681718,  0.147621, -0.716567 ,
    -0.681718, -0.147621, -0.716567 ,
    -0.850651,  0.000000, -0.525731 ,
    -0.688191,  0.587785, -0.425325 ,
    -0.587785,  0.425325, -0.688191 ,
    -0.425325,  0.688191, -0.587785 ,
    -0.425325, -0.688191, -0.587785 ,
    -0.587785, -0.425325, -0.688191 ,
    -0.688191, -0.587785, -0.425325
  );
  return(matrix(normals_raw, ncol = 3L, byrow = TRUE));
}


#quadf = '~/data/q2_pak/models/items/quaddama/tris.md2'
#md2q = read.quake.md2(quadf);

#' @title Check whether object is Quake 2 MD2 model
#'
#' @param x any R object
#'
#' @export
is.quakemodel_md2 <- function(x) inherits(x, 'quakemodel_md2')

#' @title Check whether object is Quake 1 MDL model
#'
#' @param x any R object
#'
#' @export
is.quakemodel_mdl <- function(x) inherits(x, 'quakemodel_mdl')


#' @title Check whether object is Quake 1 or 2 alias model
#'
#' @param x any R object
#'
#' @export
is.quakemodel <- function(x) inherits(x, 'quakemodel')


#' @title Visualize Quake or Quake II alias model.
#'
#' @param model a quakemodel instance, can be from a Quake 1 MDL file from a Quake II MD2 model file. Alternatively, a character string which will be interpreted as a file to load.
#'
#' @param texture_file character string, path to model skin. Q2 MD2 only, Q1 models include the skin.
#'
#' @param frame_idx integer, which frame to use for vertex positions. A model contains many vertex positions if it includes model animation data.
#'
#' @return fs.surface instance, the mesh.
#'
#' @export
#' @importFrom rgl open3d material3d shade3d
vis.quakemodel <- function(model, texture_file = NULL, frame_idx=1L) {
  md2 = model;
  if(requireNamespace('rgl', quietly = TRUE)) {
    if(!(is.quakemodel_md2(md2) | is.quakemodel_mdl(md2))) {
      if(is.character(md2)) {
        if(endsWith(md2, ".md2")) {
          md2 = read.quake.md2(md2);
        } else {
          md2 = read.quake.mdl(md2);
        }
      } else {
        stop("Parameter 'model' must be quakemodel or path to a model file as character string.");
      }
    }
    sf = list('faces'=(md2$triangles$vertex + 1L), 'vertices'=md2$frames[[frame_idx]]$vertex_coords);
    class(sf) = c(class(sf), 'fs.surface');

    tm = rgl::tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = FALSE);

    material = rgl::material3d();


    rgl::open3d();

    if(is.null(texture_file)) {
      material$color = '#333333';
      material$specular = '#AAAAAA';
      material$fog = FALSE;
      rgl::shade3d(tm, material = material)
    } else {
      if(! file.exists(texture_file)) {
        stop(sprintf("Texture file '%s' not readable.\n", texture_file));
      }
      material$color = '#FFFFFF';
      material$texture = texture_file;
      material$specular = '#000000';

      #scale_w = md2$header$skinwidth;
      #scale_h = md2$header$skinheight;
      #scale_w = dim(readbitmap::read.bitmap(texture_file))[1];
      #scale_h = dim(readbitmap::read.bitmap(texture_file))[2];
      #scale_w = 1.0;
      #scale_h = 1.0;

      texcoords = cbind((1.0 - md2$texcoords$s), (1.0 - md2$texcoords$t));
      tm = rgl::tmesh3d(t(sf$vertices), t(sf$faces), homogeneous = FALSE, material = material, texcoords = texcoords);
      rgl::shade3d(tm);
    }
    return(sf);
  } else {
    stop("Visualization requires the 'rgl' package to be installed.");
  }
}


