var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [1, 85, 4, 4, 84, 6, 7, 51, 47, 47, 11, 28, 13, 14, 15, 28, 28, 18, 26, 23, 21, 22, 23, 24, 25, 26, 27, 28, 33, 33, 31, 32, 33, 47, 35, 39, 37, 38, 39, 44, 43, 42, 43, 44, 45, 46, 47, 50, 50, 50,
 51, 56, 54, 54, 56, 56, 84, 58, 59, 60, 81, 63, 63, 81, 66, 66, 80, 68, 69, 80, 71, 79, 77, 77, 77, 77, 77, 78, 79, 80, 81, 82, 83, 84, 85, , 89, 89, 89, 101, 92, 92, 96, 95, 95, 96, 97, 98, 99, 101,
 101, 104, 103, 104, 105, , 107, 113, 113, 113, 113, 113, 113, 114, 115, , 117, 122, 122, 120, 122, 122, 123, 124, 129, 128, 128, 128, 129, 130, 131, , 133, 134, 135, 209, 137, 145, 139, 144, 143, 142, 143, 144, 145, 209, 188, 188, 185, 172,
 151, 168, 153, 154, 155, 168, 157, 165, 162, 160, 161, 162, 163, 164, 165, 166, 168, 168, 172, 170, 171, 172, 185, 174, 175, 181, 177, 181, 179, 180, 181, 182, 183, 185, 185, 188, 187, 188, 192, 190, 192, 192, 207, 195, 195, 196, 206, 206, 200, 200,
 204, 203, 203, 204, 205, 206, 207, 208, 209, 211, 211, 212, , 216, 216, 216, 284, 218, 221, 220, 221, 276, 261, 261, 225, 230, 227, 230, 229, 230, 231, 232, 259, 256, 235, 252, 252, 238, 239, 240, 252, 242, 250, 247, 245, 246, 247, 248, 249, 250,
 251, 252, 256, 254, 255, 256, 259, 259, 259, 261, 261, 273, 263, 270, 270, 270, 270, 268, 270, 270, 273, 272, 273, 275, 275, 276, 284, 278, 283, 280, 281, 282, 283, 284, 285, , 291, 288, 291, 290, 291, 293, 293, 305, 301, 296, 300, 300, 299, 300,
 301, 302, 305, 304, 305, 306, , 340, 309, 310, 311, 335, 314, 314, 335, 316, 324, 322, 322, 322, 322, 322, 323, 324, 334, 327, 327, 334, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, , 370, 370, 345, 368, 364, 348, 356, 350,
 351, 353, 353, 354, 355, 356, 357, 364, 359, 360, 361, 364, 363, 364, 368, 366, 367, 368, 370, 370, 371, , 399, 375, 375, 396, 396, 378, 386, 384, 384, 384, 384, 384, 385, 386, 395, 389, 389, 395, 391, 392, 393, 394, 395, 396, 397, 398, 399, 403,
 401, 402, 403, 404, , 406, 409, 409, 409, 411, 411, 412, , 419, 418, 418, 417, 418, 419, 420, , 422, 423, 451, 450, 427, 427, 447, 447, 430, 431, 432, 446, 435, 435, 446, 437, 445, 443, 443, 443, 443, 443, 444, 445, 446, 447, 448, 449, 450,
 451, 452, , 454, 455, 510, 504, 459, 459, 504, 461, 469, 467, 467, 467, 467, 467, 468, 469, 503, 472, 472, 503, 474, 475, 476, 497, 478, 479, 493, 482, 482, 493, 488, 488, 488, 488, 488, 489, 492, 491, 492, 493, 497, 496, 496, 497, 498, 501, 500,
 501, 502, 503, 504, 507, 507, 507, 508, 509, 510, 511, 512, , 515, 515, 529, 517, 525, 523, 523, 523, 523, 523, 524, 525, 529, 527, 528, 529, 537, 532, 532, 537, 534, 535, 536, 537, 538, 539, 540, 541, 542, , 'end'];

var FunctionNotes = [412, 420, 115, 131, 123, 105, 98, 452, 371, 341, 338, 404, 85, 45, 212, 182, 542, 512, 285, 231, 306, 'end'];

var CodeTags = [[10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], [10, 6, 1, 'end'], [10, 6, 'end'], [10, 6, 'end'], [10, 6, 'end'], [10, 6, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'],
 [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], ['end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [10, 'end'], [10, 'end'], [10, 'end'], [10, 'end'],
 [10, 'end'], [10, 'end'], [10, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[412, 'end'], [410, 'end'], [420, 'end'], [413, 'end'], [115, 'end'], [114, 'end'], [131, 'end'], [129, 'end'], [122, 'end'], [121, 'end'], [120, 'end'], [117, 'end'], [105, 'end'], [104, 'end'], [103, 'end'], [101, 'end'], [99, 'end'], [97, 'end'], [452, 'end'], [451, 'end'], [423, 'end'], [450, 'end'], [449, 'end'], [431, 'end'], [371, 'end'], [370, 'end'], [343, 'end'], [345, 'end'], [341, 'end'], [340, 'end'], [339, 'end'], [337, 'end'], [310, 'end'], [309, 'end'], [332, 'end'], [330, 'end'], [329, 'end'], [404, 'end'], [403, 'end'], [402, 'end'], [399, 'end'], [398, 'end'], [393, 'end'], [391, 'end'], [85, 'end'], [1, 'end'], [0, 'end'], [84, 'end'], [83, 'end'], [59, 'end'],
 [58, 'end'], [68, 'end'], [4, 'end'], [56, 'end'], [54, 'end'], [51, 'end'], [7, 'end'], [50, 'end'], [47, 'end'], [46, 'end'], [44, 'end'], [39, 'end'], [37, 'end'], [43, 'end'], [42, 'end'], [212, 'end'], [211, 'end'], [209, 'end'], [135, 'end'], [145, 'end'], [144, 'end'], [143, 'end'], [139, 'end'], [137, 'end'], [136, 'end'], [208, 'end'], [192, 'end'], [190, 'end'], [188, 'end'], [185, 'end'], [183, 'end'], [181, 'end'], [542, 'end'], [541, 'end'], [539, 'end'], [535, 'end'], [534, 'end'], [527, 'end'], [512, 'end'], [511, 'end'], [510, 'end'], [455, 'end'], [509, 'end'], [501, 'end'], [500, 'end'], [475, 'end'], [474, 'end'], [478, 'end'], [477, 'end'], [285, 'end'],
 [284, 'end'], [276, 'end'], [221, 'end'], [275, 'end'], [273, 'end'], [272, 'end'], [261, 'end'], [259, 'end'], [232, 'end'], [230, 'end'], [270, 'end'], [306, 'end'], [305, 'end'], [300, 291, 'end'], [299, 288, 'end'], [297, 286, 'end'], [296, 290, 'end'], [294, 'end'], 'end'];
var CodeCoverage;
var SourceCoverage;

function init_file () {
  if (top.close_target_frame) {
    var backlink = document.getElementById('backlink');
    backlink.innerHTML = '[Close]<p>';
    backlink.onclick = top.close_target_frame;
  }
  colorize (true);
}

function tags_intersect (tags1, tags2) {   // tags2 = true means all tags.
  var ntags = tags1.length - 1;
  if (tags2 === true)
    return (ntags > 0);
  for (var i = 0; i < ntags; i++) {
    var tag1 = tags1[i];
    for (var j = 0; j < tags2.length; j++)
      if (tag1 == tags2[j]) return true;
  }
  return false;
}

function is_member (elt, vec) {
  for (var i = 0; i < vec.length; i++) {
    if (vec[i] == elt) return true;
  }
  return false;
}

function set_stats_with_pct(name, count, total) {
  var pct;

  if (total > 0) {
    var pct = (count * 100) / total;
    pct = pct.toFixed(1) + '&#37;';
  }
  else {
    pct = '--';
  }
  
  document.getElementById(name).innerHTML = count;

  document.getElementById(name + 'Pct').innerHTML =  pct;
}

function colorize (tags_to_show) {
  var style;

  // Compute acode coverage and colorize acode
  var total = (CodeTags ? CodeTags.length : CodeCoverage.length) - 1;
  var num_entered = 0;
  var coverage = new Array(total);

  for (var cn = 0; cn < total; cn++) {
    var covered = (CodeTags ? tags_intersect(CodeTags[cn], tags_to_show) : CodeCoverage[cn]);
    style = (covered ? ALL_COVERED : NOT_COVERED);

    var sub_style = coverage[cn];
    if (sub_style && (style != sub_style)) style = PARTLY_COVERED;

    coverage[cn] = style; // save for source coloring use below
    if (style != NOT_COVERED) num_entered++;
    var parent = CodeParents[cn];
    if (parent) {
      var sibs_style = coverage[parent];
      if (sibs_style != style) coverage[parent] = (!sibs_style ? style : PARTLY_COVERED);
    }

  var elt = document.getElementById('f0c' + cn);  // some notes don't have a matched up source.
  if (elt) elt.className = 'st' + style;
  }


  document.getElementById('acodeTotal').innerHTML = total;
  set_stats_with_pct('acodeCovered', num_entered, total);

  // Count unreached branches (aka maximal unentered forms)
  var total = coverage.length;
  var num_branches = 0;
  var parent;
  for (var cn = 0; cn < total; cn++) {
    if ((coverage[cn] == NOT_COVERED) && // not covered
        (parent = CodeParents[cn]) &&  // has a parent
        (coverage[parent] != NOT_COVERED) &&  // that's covered
        (!is_member(cn, FunctionNotes))) // and not an entry note
      num_branches++;
  }

  document.getElementById('branchUnreached').innerHTML = num_branches;


  // Colorize Source
  var total = (SourceCodeNotes ? SourceCodeNotes.length : SourceCoverage.length) - 1;
  var num_all = 0, num_partly = 0;

  for (var sn = 0; sn < total; sn++) {
    if (SourceCodeNotes) {
      var notes = SourceCodeNotes[sn];
      for (var i = 0, style = NO_DATA; i < (notes.length - 1); i++) {
        var note_style = coverage[notes[i]];
        if (style != note_style) style = (style == NO_DATA ? note_style : PARTLY_COVERED);
      }
    }
    else {
      style = SourceCoverage[sn];
    }

    switch (style) {
      case ALL_COVERED: num_all++; break;
      case PARTLY_COVERED: num_partly++; break;
    }

   document.getElementById('f0s' + sn).className = 'st' + style;

  }
  document.getElementById('srcTotal').innerHTML = total;
  set_stats_with_pct('srcEntered', num_all + num_partly, total);
  set_stats_with_pct('srcCovered', num_all, total);

  var total = FunctionNotes.length - 1;
  var num_all = 0, num_partly = 0, num_not = 0;

  for (var i = 0; i < total; i++) {
    var cn = FunctionNotes[i];
    switch (coverage[FunctionNotes[i]]) {
      case ALL_COVERED: num_all++; break;
      case PARTLY_COVERED: num_partly++; break;
      case NOT_COVERED: num_not++; break;
    }
  }

  document.getElementById('fnTotal').innerHTML = total;
  set_stats_with_pct('fnCovered', num_all, total);
  set_stats_with_pct('fnPartly', num_partly, total);
  set_stats_with_pct('fnUnentered', num_not, total);


}

function show_tags (sn) {
  tags_frame = top.frames.tagsframe;
  if (tags_frame && tags_frame.select_tags) {
    var tags = new Array();
    var outer_notes = SourceCodeNotes[sn].slice(0);
    var total = CodeTags.length - 1;
    for (cn = total - 1; cn >= 0; cn--) {
      if (is_member(CodeParents[cn], outer_notes)) {
         outer_notes.push(cn);
         var new_tags = CodeTags[cn];
         var n = new_tags.length - 1;
         for (i = 0; i < n; i++) {
           var tag = new_tags[i];
           if (!is_member(tag, tags)) tags.push(tag);
         }
      }
    }
    tags_frame.select_tags(tags);
  }
}


