var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [2, 2, 3, 37, 5, 37, 7, 37, 9, 36, 35, 12, 13, 14, 35, 16, 35, 18, 29, 21, 21, 28, 27, 25, 25, 27, 27, 28, 29, 30, 33, 32, 33, 34, 35, 36, 37, 38, , 40, 42, 42, 43, 50, 45, 50, 49, 48, 49, 50,
 51, , 53, 54, 61, 56, 57, 61, 59, 60, 61, 62, 63, 65, 65, 66, , 83, 69, 70, 80, 72, 73, 80, 75, 79, 77, 78, 79, 80, 81, 82, 83, 84, , 98, 87, 88, 95, 90, 91, 95, 93, 94, 95, 96, 97, 98, 99, ,
 101, 102, 103, , 107, 106, 107, 108, , 111, 111, 114, 114, 114, 119, 119, 118, 118, 119, 124, 121, 124, 123, 124, 125, , 'end'];

var FunctionNotes = [51, 125, 108, 103, 38, 66, 62, 84, 81, 99, 96, 'end'];

var CodeTags = [[12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], ['end'], ['end'], ['end'], ['end'],
 [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], [12, 9, 8, 6, 5, 1, 'end'], [12, 9, 8, 6, 5, 1, 'end'], [12, 9, 8, 6, 5, 1, 'end'], [12, 9, 8, 6, 5, 1, 'end'], [12, 9, 8, 6, 5, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 8, 7, 6, 5, 4, 1, 'end'], [12, 10, 9, 8, 7, 6, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 8, 7, 6, 5, 4, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], [12, 10, 9, 8, 7, 6, 5, 4, 2, 1, 'end'], 'end'];

var SourceCodeNotes = [[51, 'end'], [50, 'end'], [48, 40, 'end'], [125, 'end'], [124, 'end'], [123, 'end'], [121, 'end'], [119, 'end'], [118, 'end'], [111, 'end'], [108, 'end'], [107, 'end'], [106, 'end'], [104, 'end'], [103, 'end'], [102, 'end'], [101, 'end'], [38, 'end'], [37, 'end'], [5, 'end'], [7, 'end'], [36, 'end'], [9, 'end'], [29, 'end'], [18, 'end'], [28, 'end'], [21, 'end'], [27, 'end'], [25, 'end'], [3, 'end'], [2, 'end'], [66, 'end'], [65, 'end'], [63, 'end'], [61, 'end'], [54, 'end'], [54, 'end'], [53, 'end'], [60, 'end'], [59, 'end'], [57, 'end'], [56, 'end'], [84, 'end'], [83, 'end'], [82, 'end'], [80, 'end'], [73, 'end'], [73, 'end'], [72, 'end'], [70, 'end'],
 [69, 'end'], [79, 'end'], [75, 'end'], [78, 'end'], [77, 'end'], [99, 'end'], [98, 'end'], [97, 'end'], [95, 'end'], [91, 'end'], [91, 'end'], [90, 'end'], [94, 'end'], [93, 'end'], [88, 'end'], [87, 'end'], 'end'];
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


