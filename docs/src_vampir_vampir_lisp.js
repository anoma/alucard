var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [7, 4, 4, 4, 6, 6, 7, 9, 9, , 11, 65, 62, 14, 61, 17, 17, 20, 19, 20, 49, 25, 25, 24, 25, 43, 27, 28, 42, 33, 31, 33, 33, 36, 35, 36, 42, 41, 39, 41, 41, 42, 43, 48, 46, 46, 48, 48, 49, 54,
 53, 53, 53, 54, 59, 57, 57, 58, 59, 61, 61, 62, 63, 64, 65, 66, 67, 68, , 100, 73, 73, 73, 92, 76, 76, 79, 78, 79, 91, 81, 84, 84, 84, 85, 90, 88, 88, 90, 90, 91, 92, 97, 95, 95, 96, 97, 100, 99, 100,
 102, 102, 103, 104, 107, 106, 107, 108, 109, 110, , 112, 148, 114, 142, 142, 119, 119, 119, 136, 121, 125, 124, 124, 125, 135, 134, 129, 129, 130, 134, 133, 133, 134, 135, 136, 141, 139, 139, 140, 141, 142, 145, 144, 145, 146, 147, 148, 149, 150,
 224, 153, 153, 224, 155, 191, 157, 188, 160, 160, 161, 183, 164, 164, 170, 167, 167, 168, 170, 170, 177, 172, 176, 175, 175, 176, 177, 182, 181, 181, 181, 182, 183, 187, 185, 187, 187, 188, 189, 190, 191, 192, 193, 223, 196, 196, 223, 199, 199, 206,
 202, 202, 206, 205, 205, 206, 207, 222, 210, 210, 211, 222, 217, 217, 217, 217, 217, 218, 221, 220, 221, 222, 223, 224, 225, 226, 228, 228, 230, 230, , 235, 235, 234, 235, 236, 237, , 243, 240, 242, 242, 243, 244, 267, 262, 247, 258, 249, 258,
 251, 252, 253, 254, 255, 256, 257, 258, 261, 260, 261, 262, 267, 266, 266, 266, 267, 270, 269, 270, 271, 272, , 274, 324, 276, 277, 278, 323, 280, 318, 282, 285, 284, 285, 317, 303, 288, 299, 290, 299, 292, 293, 294, 295, 296, 297, 298, 299, 302,
 301, 302, 303, 315, 307, 306, 307, 310, 310, 310, 311, 315, 314, 314, 315, 316, 317, 318, 319, 323, 323, 322, 323, 324, 330, 329, 327, 329, 329, 330, 331, , 333, 334, 337, 336, 337, 349, 342, 342, 341, 342, 349, 344, 348, 346, 347, 348, 349, 350,
 , 352, 406, 401, 357, 357, 357, 393, 385, 363, 363, 362, 363, 381, 368, 366, 368, 368, 381, 371, 371, 381, 374, 374, 381, 376, 380, 378, 379, 380, 381, 385, 384, 384, 385, 392, 387, 391, 390, 390, 391, 392, 393, 398, 396, 396, 397, 398, 401, 400,
 401, 403, 403, 404, 405, 406, 407, 408, 409, , 413, 413, 413, 415, 415, 417, 417, , 419, 458, 455, 454, 424, 424, 427, 426, 427, 441, 429, 434, 434, 432, 434, 434, 435, 440, 440, 439, 439, 440, 441, 446, 445, 445, 445, 446, 451, 449, 449, 450,
 451, 454, 453, 454, 455, 456, 457, 458, 459, 460, 461, , 463, 496, 495, 466, 495, 470, 470, 470, 489, 475, 473, 475, 475, 476, 481, 479, 479, 481, 481, 488, 484, 484, 487, 486, 487, 488, 489, 494, 492, 492, 493, 494, 495, 496, 497, 498, 501, 500,
 501, 502, 503, 504, , 'end'];

var FunctionNotes = [9, 110, 103, 409, 404, 417, 68, 63, 461, 456, 230, 146, 189, 350, 331, 237, 504, 497, 272, 'end'];

var CodeTags = [[11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'],
 [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], [11, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[9, 'end'], [6, 'end'], [4, 'end'], [2, 'end'], [110, 'end'], [108, 'end'], [84, 'end'], [82, 'end'], [81, 'end'], [409, 'end'], [407, 'end'], [363, 'end'], [359, 'end'], [362, 'end'], [368, 'end'], [367, 'end'], [366, 'end'], [371, 'end'], [369, 'end'], [380, 'end'], [378, 'end'], [374, 'end'], [372, 'end'], [417, 'end'], [413, 'end'], [412, 'end'], [68, 'end'], [66, 'end'], [42, 'end'], [28, 'end'], [27, 'end'], [41, 'end'], [37, 'end'], [39, 'end'], [35, 'end'], [33, 'end'], [29, 'end'], [31, 'end'], [25, 'end'], [21, 'end'], [24, 'end'], [461, 'end'], [459, 'end'], [434, 'end'], [430, 'end'], [429, 'end'], [432, 'end'], [230, 'end'], [226, 'end'], [149, 'end'],
 [143, 'end'], [129, 'end'], [192, 'end'], [156, 'end'], [167, 'end'], [210, 'end'], [350, 'end'], [348, 'end'], [346, 'end'], [342, 'end'], [338, 'end'], [341, 'end'], [337, 'end'], [333, 'end'], [331, 'end'], [329, 'end'], [328, 'end'], [327, 'end'], [324, 'end'], [274, 'end'], [316, 'end'], [312, 309, 'end'], [285, 'end'], [237, 'end'], [235, 'end'], [232, 'end'], [234, 'end'], [504, 'end'], [502, 'end'], [462, 'end'], [475, 'end'], [471, 'end'], [473, 'end'], [272, 'end'], [270, 'end'], [265, 'end'], [269, 'end'], 'end'];
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


