var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [1, 2, 3, 13, 5, 7, 7, 12, 9, 10, 12, 12, 13, 14, 39, 17, 17, 39, 19, 27, 25, 25, 25, 25, 25, 26, 27, 38, 30, 30, 38, 32, 36, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, , 47, 46, 47, 48, 90, 50,
 53, 53, 53, 54, 90, 58, 57, 58, 59, 74, 61, 71, 65, 64, 65, 71, 67, 69, 69, 70, 71, 74, 73, 74, 89, 76, 77, 78, 79, 82, 81, 82, 83, 89, 87, 86, 87, 88, 89, 90, 91, 92, 124, 94, 95, 96, 121, 102, 102, 102,
 102, 102, 103, 106, 105, 106, 117, 108, 109, 112, 111, 112, 113, 117, 116, 116, 117, 121, 120, 120, 121, 122, 123, 124, 125, , 127, 188, 129, 137, 135, 135, 135, 135, 135, 136, 137, 144, 140, 140, 144, 142, 143, 144, 185, 146, 169, 148, 165, 150,
 151, 152, 165, 157, 155, 156, 157, 158, 159, 162, 161, 162, 163, 165, 165, 169, 167, 168, 169, 180, 180, 174, 173, 174, 175, 176, 177, 178, 180, 180, 181, 185, 184, 184, 185, 186, 187, 188, 189, , 192, 192, 219, 194, 195, 199, 198, 198, 199, 200,
 219, 206, 206, 206, 206, 206, 207, 210, 209, 210, 218, 212, 213, 214, 218, 217, 217, 218, 219, 220, 221, 222, 223, , 225, 226, 227, 242, 229, 232, 231, 232, 235, 234, 235, 241, 238, 238, 239, 241, 241, 242, 243, 244, , 'end'];

var FunctionNotes = [43, 223, 189, 177, 244, 125, 92, 'end'];

var CodeTags = [[12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], [6, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'],
 [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], 'end'];

var SourceCodeNotes = [[43, 'end'], [42, 'end'], [41, 'end'], [13, 'end'], [3, 'end'], [1, 'end'], [12, 'end'], [10, 'end'], [7, 'end'], [6, 'end'], [5, 'end'], [36, 'end'], [35, 'end'], [32, 'end'], [31, 'end'], [223, 'end'], [222, 'end'], [221, 'end'], [199, 'end'], [195, 'end'], [198, 'end'], [196, 'end'], [213, 'end'], [189, 'end'], [188, 'end'], [127, 'end'], [187, 'end'], [180, 'end'], [178, 'end'], [176, 'end'], [173, 'end'], [146, 'end'], [142, 'end'], [141, 'end'], [244, 'end'], [243, 'end'], [227, 'end'], [226, 'end'], [125, 'end'], [124, 'end'], [92, 'end'], [91, 'end'], [53, 'end'], [50, 'end'], [82, 'end'], [79, 'end'], [77, 'end'], [81, 'end'], [123, 'end'], [95, 'end'],
 [94, 'end'], [112, 'end'], [109, 'end'], [108, 'end'], [111, 'end'], 'end'];
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


