var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 11, 13, 13, 14, 15, , 51, 51, 19, 42, 21, 22, 42, 24, 25, 26, 41, 41, 29, 37, 34, 32, 33, 34, 35, 36, 37, 38, 41, 40, 41, 42, 51, 48, 45, 47, 47, 48, 49, 50,
 51, 52, , 54, 55, , 62, 61, 59, 61, 61, 62, 63, , 65, 84, 68, 68, 83, 71, 71, 83, 73, 82, 75, 82, 77, 82, 79, 82, 81, 82, 83, 84, 85, 86, , 93, 92, 92, 91, 92, 93, 94, , 103, 97, 103, 103, 100,
 103, 102, 103, 115, 113, 113, 107, 113, 109, 113, 111, 113, 113, 115, 115, 119, 117, 118, 119, 120, 121, 122, 127, 126, 126, 126, 127, 128, 129, , 138, 132, 138, 138, 135, 138, 137, 138, 139, 140, 145, 144, 144, 144, 145, 146, 147, , 184, 150,
 184, 179, 179, 179, 155, 156, 178, 158, 178, 160, 177, 165, 163, 164, 165, 166, 167, 170, 169, 170, 171, 177, 177, 174, 175, 176, 177, 178, 179, 184, 181, 184, 184, 184, 185, 186, , 190, 190, 190, 200, 196, 196, 194, 195, 196, 197, 198, 199, 200,
 201, 202, , 204, 222, 206, 222, 218, 218, 218, 218, 218, 218, 218, 218, 216, 218, 218, 219, 222, 221, 222, , 238, 225, 238, 235, 235, 229, 235, 235, 235, 235, 235, 235, 236, 237, 238, , 240, 276, 242, 276, 244, 276, 276, 276, 275, 275, 275,
 254, 252, 253, 254, 255, 256, 259, 258, 259, 260, 268, 268, 263, 264, 265, 268, 267, 268, 274, 270, 274, 272, 273, 274, 275, 276, 277, 278, 279, , 286, 282, 285, 285, 285, 286, 287, , 294, 290, 293, 293, 293, 294, 295, , 297, 298, , 'end'];

var FunctionNotes = [287, 295, 86, 94, 55, 63, 298, 279, 15, 147, 139, 238, 129, 121, 222, 202, 198, 186, 52, 49, 'end'];

var CodeTags = [[5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'],
 [10, 6, 1, 'end'], [10, 6, 1, 'end'], [10, 6, 1, 'end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [9, 5, 'end'], [9, 5, 'end'],
 [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], ['end'], ['end'], [9, 5, 'end'], ['end'], ['end'], ['end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], [10, 6, 5, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[287, 'end'], [280, 'end'], [295, 'end'], [288, 'end'], [86, 'end'], [84, 'end'], [65, 'end'], [71, 'end'], [68, 'end'], [94, 'end'], [87, 'end'], [55, 'end'], [54, 'end'], [53, 'end'], [63, 'end'], [56, 'end'], [298, 'end'], [297, 'end'], [296, 'end'], [279, 'end'], [277, 'end'], [276, 'end'], [246, 'end'], [275, 'end'], [247, 'end'], [15, 'end'], [14, 'end'], [147, 'end'], [145, 'end'], [138, 'end'], [133, 'end'], [132, 'end'], [135, 'end'], [137, 'end'], [238, 'end'], [225, 'end'], [224, 'end'], [237, 'end'], [236, 'end'], [129, 'end'], [127, 'end'], [120, 'end'], [119, 'end'], [118, 'end'], [115, 'end'], [113, 'end'], [112, 'end'], [103, 'end'], [95, 'end'], [222, 'end'],
 [221, 'end'], [220, 'end'], [206, 'end'], [205, 'end'], [204, 'end'], [203, 'end'], [219, 'end'], [202, 'end'], [200, 'end'], [197, 'end'], [196, 'end'], [191, 'end'], [195, 'end'], [186, 'end'], [185, 'end'], [181, 'end'], [179, 'end'], [152, 'end'], [158, 'end'], [52, 'end'], [51, 'end'], [50, 'end'], [48, 'end'], [47, 'end'], [45, 'end'], [19, 'end'], 'end'];
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


