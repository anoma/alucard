var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [2, 2, 6, 5, 5, 6, 7, 49, 9, 49, 12, 12, 34, 14, 15, 18, 17, 18, 22, 22, 21, 22, 23, 34, 25, 33, 31, 31, 31, 31, 31, 32, 33, 34, 48, 43, 37, 38, 41, 40, 41, 42, 43, 44, 48, 47, 47, 48, 49, 50,
 51, 52, 53, 54, 86, 81, 57, 58, 79, 60, 61, 79, 63, 64, 79, 71, 70, 68, 69, 70, 71, 79, 73, 74, 79, 76, 77, 79, 79, 81, 81, 82, 84, 84, 85, 86, 87, , 188, 187, 187, 187, 94, 94, 98, 97, 97, 98, 99, 181,
 103, 102, 103, 104, 118, 110, 110, 110, 110, 110, 111, 114, 113, 114, 118, 117, 117, 118, 178, 121, 121, 178, 123, 176, 126, 126, 174, 128, 174, 130, 173, 132, 148, 134, 138, 136, 137, 138, 148, 141, 141, 147, 143, 144, 147, 146, 147, 148, 149, 163,
 155, 155, 155, 155, 155, 156, 159, 158, 159, 163, 162, 162, 163, 173, 165, 166, 171, 168, 171, 170, 171, 172, 173, 174, 175, 176, 177, 178, 181, 180, 181, 182, 183, 184, 185, 187, 187, 188, , 191, 191, 423, 193, 196, 195, 196, 242, 241, 200, 200,
 233, 203, 203, 230, 205, 208, 207, 208, 209, 210, 230, 216, 216, 216, 216, 216, 217, 220, 219, 220, 229, 222, 229, 224, 227, 226, 227, 228, 229, 230, 233, 232, 233, 234, 240, 236, 239, 238, 239, 240, 241, 242, 243, 244, 271, 271, 271, 270, 266, 250,
 251, 253, 253, 254, 255, 258, 257, 258, 259, 266, 261, 262, 263, 266, 265, 266, 270, 268, 269, 270, 271, 277, 273, 277, 276, 276, 277, 278, 423, 280, 288, 286, 286, 286, 286, 286, 287, 288, 294, 291, 291, 294, 293, 294, 309, 297, 297, 309, 299, 300,
 303, 302, 303, 307, 307, 306, 307, 308, 309, 422, 311, 387, 313, 320, 320, 319, 317, 319, 319, 320, 387, 323, 323, 354, 325, 344, 328, 328, 341, 330, 338, 336, 336, 336, 336, 336, 337, 338, 341, 340, 341, 344, 343, 344, 354, 352, 347, 348, 351, 350,
 351, 352, 353, 354, 355, 356, 357, 358, 386, 386, 386, 362, 363, 385, 365, 366, 368, 368, 369, 370, 373, 372, 373, 374, 382, 382, 377, 382, 379, 380, 381, 382, 385, 384, 385, 386, 387, 388, 413, 390, 409, 393, 393, 409, 395, 408, 397, 408, 403, 403,
 403, 403, 403, 404, 407, 406, 407, 408, 409, 413, 412, 412, 413, 414, 417, 416, 417, 418, 422, 421, 421, 422, 423, 424, 425, 426, 427, 454, 446, 430, 446, 432, 433, 434, 446, 436, 444, 441, 439, 440, 441, 442, 443, 444, 445, 446, 451, 448, 449, 451,
 451, 454, 454, 454, 455, , 'end'];

var FunctionNotes = [188, 184, 455, 426, 243, 357, 87, 52, 'end'];

var CodeTags = [[6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], 'end'];

var SourceCodeNotes = [[188, 'end'], [88, 'end'], [187, 'end'], [185, 'end'], [183, 'end'], [176, 'end'], [123, 'end'], [171, 'end'], [165, 'end'], [168, 'end'], [148, 'end'], [132, 'end'], [138, 'end'], [136, 'end'], [147, 'end'], [143, 'end'], [141, 'end'], [103, 'end'], [102, 'end'], [455, 'end'], [454, 'end'], [427, 'end'], [425, 'end'], [277, 'end'], [273, 'end'], [271, 'end'], [244, 'end'], [242, 'end'], [196, 'end'], [241, 'end'], [240, 'end'], [239, 'end'], [209, 'end'], [208, 'end'], [227, 'end'], [276, 'end'], [417, 'end'], [416, 'end'], [387, 'end'], [311, 'end'], [386, 'end'], [358, 'end'], [356, 'end'], [352, 'end'], [351, 'end'], [347, 'end'], [384, 'end'], [320, 'end'], [319, 'end'], [307, 'end'],
 [303, 'end'], [299, 'end'], [87, 'end'], [86, 'end'], [53, 'end'], [51, 'end'], [43, 'end'], [41, 'end'], [37, 'end'], [22, 'end'], [18, 'end'], [14, 'end'], 'end'];
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


