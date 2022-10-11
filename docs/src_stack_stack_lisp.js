var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [5, 5, 3, 5, 5, 6, 13, 13, 13, 13, 11, 13, 13, 14, 25, 16, 19, 18, 19, 24, 21, 24, 23, 24, 25, 26, 27, , 29, 30, 31, 33, 33, 34, 36, 36, , 38, 79, 46, 46, 42, 46, 46, 45, 46, 47, 79, 59, 59,
 51, 52, 59, 54, 55, 59, 59, 58, 59, 60, 61, 62, 78, 71, 65, 71, 67, 68, 71, 71, 71, 72, 78, 74, 75, 77, 77, 78, 79, 100, 91, 82, 91, 91, 91, 86, 87, 88, 91, 90, 91, 92, 100, 94, 99, 96, 97, 98, 99, 100,
 101, 102, , 118, 108, 108, 107, 108, 114, 110, 113, 113, 113, 114, 118, 117, 117, 118, 125, 120, 124, 123, 123, 124, 125, 130, 129, 129, 129, 130, 135, 133, 133, 134, 135, 139, 137, 139, 139, 142, 141, 142, 143, 144, 147, 146, 147, 148, 149, 150,
 , 161, 159, 154, 155, 158, 157, 158, 159, 160, 161, , 172, 172, 165, 172, 167, 168, 172, 170, 172, 172, 173, 174, 175, , 181, 178, 179, 180, 181, 182, 184, 184, , 186, 262, 261, 190, 190, 191, 258, 245, 195, 195, 198, 197, 198, 232, 200,
 231, 202, 213, 204, 213, 206, 207, 208, 209, 210, 211, 212, 213, 216, 215, 216, 218, 218, 230, 224, 223, 222, 223, 224, 225, 230, 229, 229, 229, 230, 231, 232, 235, 234, 235, 241, 237, 240, 240, 240, 241, 245, 244, 244, 245, 252, 248, 248, 251, 250,
 251, 252, 257, 256, 256, 256, 257, 258, 261, 260, 261, 262, 263, 264, 267, 266, 267, 268, 269, 270, , 280, 273, 274, 276, 276, 278, 278, 279, 280, , 282, 284, 284, 285, 286, 287, , 291, 290, 291, , 293, 294, 298, 296, 297, 298, 299, 300,
 , 302, 330, 315, 305, 306, 308, 308, 315, 310, 315, 312, 315, 315, 315, 316, 330, 318, 328, 320, 322, 322, 328, 324, 328, 328, 328, 328, 329, 330, 331, 332, , 344, 335, 344, 337, 338, 344, 344, 341, 342, 344, 344, 345, 360, 350, 348, 349, 350,
 360, 358, 353, 358, 355, 358, 358, 358, 359, 360, 361, 362, , 364, 365, 366, 369, 368, 369, 386, 374, 374, 373, 374, 386, 385, 380, 379, 379, 380, 383, 383, 383, 385, 385, 386, 387, , 399, 392, 391, 392, 399, 399, 399, 396, 399, 398, 399, 400,
 401, 402, , 404, 409, 406, 407, 408, 409, 423, 413, 413, 413, 423, 417, 417, 417, 423, 422, 420, 422, 422, 423, 424, , 'end'];

var FunctionNotes = [270, 263, 150, 143, 280, 161, 36, 184, 291, 287, 424, 387, 300, 27, 332, 102, 402, 175, 362, 'end'];

var CodeTags = [[13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [13, 12, 6, 4, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'],
 [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 12, 6, 4, 0, 'end'],
 [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'],
 [0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'],
 [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], ['end'], ['end'], ['end'], ['end'], [0, 'end'], [0, 'end'], [0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [12, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [0, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 1, 0, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 1, 0, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 1, 0, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 1, 0, 'end'], [13, 'end'], [13, 'end'], [13, 'end'], [13, 'end'], [13, 'end'], [13, 'end'], [13, 'end'], [13, 'end'],
 [13, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 12, 6, 4, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'],
 [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'],
 [13, 0, 'end'], [13, 0, 'end'], [13, 0, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], 'end'];

var SourceCodeNotes = [[270, 'end'], [268, 'end'], [185, 'end'], [235, 'end'], [234, 'end'], [231, 'end'], [226, 'end'], [200, 'end'], [198, 'end'], [197, 'end'], [195, 'end'], [194, 'end'], [240, 'end'], [239, 'end'], [237, 'end'], [150, 'end'], [148, 'end'], [140, 'end'], [108, 'end'], [105, 'end'], [107, 'end'], [113, 'end'], [111, 'end'], [110, 'end'], [280, 'end'], [279, 'end'], [276, 'end'], [273, 'end'], [161, 'end'], [160, 'end'], [158, 'end'], [154, 'end'], [36, 'end'], [34, 'end'], [31, 'end'], [29, 'end'], [184, 'end'], [182, 'end'], [180, 'end'], [178, 'end'], [291, 'end'], [290, 'end'], [287, 'end'], [286, 'end'], [285, 'end'], [424, 'end'], [413, 'end'], [387, 'end'], [385, 'end'], [300, 'end'],
 [299, 'end'], [298, 'end'], [297, 'end'], [294, 'end'], [27, 'end'], [26, 'end'], [25, 'end'], [24, 'end'], [19, 'end'], [14, 'end'], [6, 'end'], [332, 'end'], [331, 'end'], [330, 'end'], [316, 'end'], [308, 'end'], [329, 'end'], [322, 'end'], [102, 'end'], [101, 'end'], [100, 'end'], [99, 'end'], [98, 'end'], [98, 'end'], [97, 'end'], [92, 'end'], [88, 'end'], [47, 'end'], [77, 'end'], [75, 'end'], [62, 'end'], [60, 'end'], [52, 'end'], [55, 'end'], [72, 'end'], [68, 'end'], [402, 'end'], [401, 'end'], [400, 'end'], [392, 'end'], [175, 'end'], [174, 'end'], [173, 'end'], [168, 'end'], [362, 'end'], [361, 'end'], [360, 'end'], [350, 'end'], [349, 'end'], [345, 'end'],
 [338, 'end'], [342, 'end'], [359, 'end'], 'end'];
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


