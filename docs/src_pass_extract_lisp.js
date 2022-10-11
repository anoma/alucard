var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [1, 9, 7, 7, 7, 7, 7, 8, 9, 21, 11, 12, 13, 21, 15, 19, 18, 18, 19, 20, 21, 29, 23, 24, 25, 29, 28, 28, 29, 30, 31, 32, , 61, 61, 36, 37, 59, 56, 40, 56, 42, 43, 44, 56, 46, 54, 51, 49, 50,
 51, 52, 53, 54, 55, 56, 59, 58, 59, 61, 61, 87, 63, 64, 65, 66, 87, 73, 73, 70, 71, 73, 73, 85, 75, 76, 77, 78, 81, 80, 81, 85, 84, 84, 85, 86, 87, 88, , 90, 92, 92, 98, 95, 95, 98, 97, 98, 101, 100,
 101, 102, 103, , 106, 106, 107, 117, 117, 111, 111, 115, 114, 114, 115, 116, 117, 146, 120, 120, 121, 143, 123, 143, 127, 126, 127, 128, 142, 134, 134, 134, 134, 134, 135, 138, 137, 138, 142, 141, 141, 142, 143, 144, 145, 146, 147, 148, , 150,
 153, 153, 153, 154, 155, , 158, 158, 163, 160, 161, 162, 163, 164, , 167, 167, 168, 169, 196, 196, 196, 195, 174, 175, 195, 177, 178, 179, 194, 181, 189, 186, 184, 185, 186, 187, 188, 189, 190, 194, 192, 194, 194, 195, 196, 197, 281, 204, 200,
 201, 204, 204, 204, 205, 221, 213, 212, 209, 212, 212, 212, 213, 217, 215, 216, 217, 221, 219, 220, 221, 234, 223, 234, 225, 226, 232, 228, 229, 232, 231, 232, 233, 234, 235, 281, 238, 238, 277, 240, 241, 242, 243, 277, 246, 246, 276, 250, 249, 250,
 251, 276, 253, 254, 275, 257, 257, 275, 260, 260, 274, 262, 270, 268, 268, 268, 268, 268, 269, 270, 274, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, , 284, 285, 307, 287, 295, 289, 290, 292, 292, 293, 294, 295, 296, 304, 298, 304, 300,
 301, 302, 304, 304, 307, 306, 307, 311, 311, 311, 311, 370, 313, 334, 315, 316, 334, 318, 324, 324, 321, 322, 324, 324, 333, 328, 327, 328, 329, 333, 331, 332, 333, 334, 335, 349, 338, 338, 349, 344, 344, 344, 344, 344, 345, 348, 347, 348, 349, 361,
 356, 356, 356, 354, 355, 356, 357, 361, 360, 360, 361, 362, 363, 364, 370, 366, 367, 368, 369, 370, 371, , 373, 383, 383, 376, 383, 383, 383, 380, 382, 382, 383, 384, 385, 386, , 393, 393, 393, 393, 393, 393, 394, 395, 398, 398, 398, 425, 400,
 414, 402, 403, 404, 414, 406, 411, 408, 410, 410, 411, 412, 414, 414, 416, 416, 417, 418, 422, 420, 421, 422, 425, 424, 425, 426, , 'end'];

var FunctionNotes = [386, 282, 235, 197, 168, 103, 32, 148, 371, 88, 426, 394, 155, 164, 'end'];

var CodeTags = [['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], [6, 'end'], ['end'], ['end'], [6, 'end'], [6, 'end'], 'end'];

var SourceCodeNotes = [[386, 'end'], [385, 'end'], [384, 'end'], [383, 'end'], [382, 'end'], [381, 'end'], [282, 'end'], [281, 'end'], [197, 'end'], [196, 'end'], [169, 'end'], [167, 'end'], [235, 'end'], [234, 'end'], [223, 'end'], [233, 'end'], [232, 'end'], [229, 'end'], [228, 'end'], [231, 'end'], [226, 'end'], [225, 'end'], [221, 'end'], [220, 'end'], [219, 'end'], [205, 'end'], [204, 'end'], [201, 'end'], [217, 'end'], [216, 'end'], [213, 'end'], [212, 'end'], [209, 'end'], [280, 'end'], [279, 'end'], [242, 'end'], [241, 'end'], [250, 'end'], [247, 'end'], [249, 'end'], [253, 'end'], [272, 'end'], [103, 'end'], [102, 'end'], [101, 'end'], [100, 'end'], [98, 'end'], [97, 'end'], [95, 'end'], [92, 'end'],
 [90, 'end'], [32, 'end'], [31, 'end'], [24, 'end'], [12, 'end'], [148, 'end'], [147, 'end'], [145, 'end'], [120, 'end'], [127, 'end'], [126, 'end'], [371, 'end'], [370, 'end'], [369, 'end'], [367, 'end'], [366, 'end'], [311, 'end'], [308, 'end'], [306, 'end'], [364, 'end'], [363, 'end'], [356, 'end'], [355, 'end'], [354, 'end'], [334, 'end'], [316, 'end'], [316, 'end'], [315, 'end'], [313, 'end'], [312, 'end'], [332, 'end'], [331, 'end'], [324, 'end'], [318, 'end'], [322, 'end'], [329, 'end'], [328, 'end'], [325, 'end'], [327, 'end'], [88, 'end'], [87, 'end'], [66, 'end'], [64, 'end'], [63, 'end'], [61, 'end'], [60, 'end'], [58, 'end'], [86, 'end'], [85, 'end'], [84, 'end'],
 [81, 'end'], [77, 'end'], [75, 'end'], [73, 'end'], [71, 'end'], [70, 'end'], [426, 'end'], [425, 'end'], [422, 'end'], [418, 'end'], [418, 'end'], [421, 'end'], [420, 'end'], [424, 'end'], [423, 'end'], [398, 'end'], [395, 'end'], [393, 'end'], [155, 'end'], [154, 'end'], [164, 'end'], [163, 'end'], [158, 'end'], [162, 'end'], [161, 'end'], [160, 'end'], 'end'];
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


