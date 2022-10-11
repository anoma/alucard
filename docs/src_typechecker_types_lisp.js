var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [2, 2, 4, 4, 5, 6, , 11, 9, 10, 11, 12, , 15, 15, 20, 20, 18, 19, 20, 21, 22, , 25, 25, 27, 27, 28, 29, , 32, 32, 33, , 44, 38, 38, 38, 44, 42, 41, 42, 43, 44, 45, 46, 64, 64, 50, 50,
 54, 53, 53, 54, 55, 61, 59, 59, 59, 61, 61, 62, 63, 64, 65, , 68, 68, 70, 70, 71, 72, , 77, 75, 77, 77, 79, 79, 80, , 98, 84, 84, 98, 95, 89, 89, 89, 95, 93, 92, 93, 94, 95, 96, 97, 98, 99, 100,
 , 102, , 109, 105, 107, 107, 108, 109, 117, 117, 114, 113, 114, 115, 117, 117, 118, , 121, 121, 122, , 125, 125, 140, 136, 128, 130, 130, 131, 136, 135, 135, 135, 136, 137, 138, 140, 140, 141, 142, , 146, 146, 146, 195, 148, 191, 188,
 151, 187, 187, 155, 155, 156, 186, 159, 159, 162, 161, 162, 180, 164, 173, 166, 173, 168, 173, 170, 173, 173, 173, 174, 179, 177, 177, 179, 179, 180, 185, 184, 184, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, , 199, 212,
 211, 204, 204, 204, 208, 208, 207, 208, 209, 210, 211, 212, , 215, 215, 233, 233, 220, 220, 220, 230, 223, 223, 227, 226, 226, 227, 228, 230, 230, 231, 232, 233, 234, 235, , 238, 238, 239, , 246, 242, 245, 245, 245, 246, 247, , 265, 252,
 252, 252, 259, 259, 255, 257, 257, 258, 259, 260, 261, 265, 263, 264, 265, 266, , 268, , 273, 273, 272, 273, 275, 275, 276, , 279, 279, 280, , 285, 283, 285, 285, 287, 287, 288, , 295, 294, 294, 293, 294, 295, 296, , 299, 299, 300,
 301, , 304, 304, 305, 306, , 308, , 316, 311, 312, 316, 315, 315, 316, 317, 318, , 321, 321, 323, 323, 324, 325, , 328, 328, 330, 330, 331, 332, , 334, 337, 337, 337, 339, 339, 340, , 343, 343, 355, 355, 346, 352, 352, 351, 351,
 351, 352, 353, 354, 355, 356, 357, , 359, 362, 362, 362, 364, 364, 365, , 368, 368, 370, 370, 371, 372, , 375, 375, 376, 377, , 380, 380, 381, 382, , 'end'];

var FunctionNotes = [102, 268, 308, 365, 306, 280, 72, 142, 29, 235, 65, 301, 239, 332, 100, 325, 318, 266, 296, 340, 377, 122, 6, 357, 212, 382, 33, 372, 22, 12, 276, 288, 80, 247, 197, 193, 189, 118, 'end'];

var CodeTags = [['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'],
 [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], ['end'], ['end'],
 [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [12, 8, 6, 'end'], ['end'],
 ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[102, 268, 308, 'end'], [307, 'end'], [267, 'end'], [101, 'end'], [365, 'end'], [363, 'end'], [306, 280, 235, 142, 72, 65, 29, 'end'], [332, 325, 318, 301, 266, 239, 100, 'end'], [296, 'end'], [289, 'end'], [340, 'end'], [338, 'end'], [377, 357, 212, 122, 6, 'end'], [199, 'end'], [198, 'end'], [382, 372, 33, 22, 12, 'end'], [276, 'end'], [274, 'end'], [288, 'end'], [286, 'end'], [80, 'end'], [78, 'end'], [247, 'end'], [240, 'end'], [197, 'end'], [195, 'end'], [192, 'end'], [173, 'end'], [171, 'end'], [166, 'end'], [164, 'end'], [170, 'end'], [168, 'end'], [118, 'end'], [117, 'end'], [115, 'end'], [109, 'end'], [108, 'end'], 'end'];
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


