var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [1, 2, 3, 13, 5, 13, 7, 9, 9, 12, 11, 12, 13, 21, 15, 16, 17, 20, 19, 20, 21, 38, 24, 24, 29, 26, 27, 28, 29, 31, 31, 37, 36, 36, 36, 36, 37, 38, 39, 40, , 42, 49, 44, 45, 46, 47, 48, 49, 50,
 51, 52, , 54, 55, 58, 57, 58, 59, 60, 61, , 142, 142, 65, 73, 67, 68, 70, 70, 71, 72, 73, 74, 82, 76, 77, 78, 82, 80, 82, 82, 140, 84, 85, 114, 112, 88, 89, 110, 110, 92, 93, 110, 95, 96, 110, 98, 99, 110,
 101, 102, 104, 104, 106, 106, 110, 108, 109, 110, 112, 112, 113, 114, 115, 134, 117, 132, 122, 121, 121, 122, 123, 124, 125, 132, 128, 128, 129, 130, 131, 132, 133, 134, 136, 136, 140, 138, 139, 140, 142, 142, 143, , 145, 148, 147, 148, 152, 151,
 151, 152, 156, 154, 155, 156, 157, , 164, 160, 163, 162, 163, 164, , 166, 167, 183, 170, 170, 171, 172, 176, 174, 175, 176, 182, 178, 181, 180, 181, 182, 183, 184, , 186, 191, 188, 189, 190, 191, 197, 193, 194, 196, 196, 197, 198, , 200,
 201, 202, 203, 204, , 212, 207, 208, 209, 211, 211, 212, , 220, 220, 220, 220, 220, 220, 220, 221, 224, 223, 224, 226, 226, 233, 230, 230, 230, 232, 232, 233, 234, 235, , 'end'];

var FunctionNotes = [212, 235, 143, 123, 130, 164, 40, 198, 157, 61, 52, 204, 184, 171, 'end'];

var CodeTags = [['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[212, 'end'], [211, 'end'], [209, 'end'], [208, 'end'], [206, 'end'], [235, 'end'], [234, 'end'], [223, 'end'], [143, 'end'], [142, 'end'], [141, 'end'], [136, 'end'], [134, 'end'], [132, 'end'], [124, 'end'], [122, 'end'], [121, 'end'], [116, 'end'], [85, 'end'], [164, 'end'], [163, 'end'], [40, 'end'], [39, 'end'], [27, 'end'], [9, 'end'], [8, 'end'], [7, 'end'], [11, 'end'], [2, 'end'], [198, 'end'], [197, 'end'], [191, 'end'], [196, 'end'], [194, 'end'], [157, 'end'], [156, 'end'], [155, 'end'], [148, 'end'], [151, 'end'], [61, 'end'], [60, 'end'], [52, 'end'], [51, 'end'], [204, 'end'], [203, 'end'], [202, 'end'], [184, 'end'], [183, 'end'], [167, 'end'], [182, 'end'],
 [181, 'end'], [176, 'end'], [172, 'end'], [170, 'end'], [175, 'end'], 'end'];
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


