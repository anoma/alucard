var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [28, 2, 25, 21, 5, 6, 7, 21, 9, 21, 11, 19, 13, 14, 16, 16, 17, 18, 19, 20, 21, 25, 23, 24, 25, 28, 28, 28, 29, 30, , 33, 33, 34, 35, 55, 37, 50, 50, 40, 41, 42, 50, 44, 48, 46, 47, 48, 49, 50,
 54, 52, 54, 54, 55, 60, 57, 59, 59, 60, 69, 62, 69, 64, 65, 68, 67, 68, 69, 70, , 73, 73, 74, 75, 103, 77, 78, 79, 94, 94, 82, 90, 87, 85, 86, 87, 88, 89, 90, 91, 94, 93, 94, 100, 96, 100, 98, 99, 100,
 103, 103, 103, 104, 105, , 135, 135, 135, 110, 111, 134, 130, 114, 130, 119, 117, 118, 119, 120, 121, 124, 123, 124, 125, 130, 127, 128, 129, 130, 134, 132, 133, 134, 135, 136, 137, , 140, 140, 143, 142, 143, 144, 145, 173, 173, 148, 165, 150,
 151, 152, 165, 154, 155, 157, 157, 158, 159, 162, 161, 162, 163, 165, 165, 171, 167, 168, 171, 170, 171, 173, 173, 174, 175, , 179, 178, 179, 180, , 182, 192, 186, 185, 186, 190, 188, 189, 190, 191, 192, 195, 194, 195, 196, 197, 198, 232, 200,
 201, 230, 228, 228, 210, 209, 207, 208, 209, 210, 227, 212, 213, 227, 215, 216, 227, 218, 219, 227, 227, 222, 223, 227, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, , 237, 237, 238, , 'end'];

var FunctionNotes = [238, 70, 34, 234, 196, 137, 30, 175, 144, 105, 74, 180, 'end'];

var CodeTags = [[12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'],
 [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'],
 [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [2, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'],
 [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], [12, 8, 6, 2, 'end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[238, 'end'], [237, 'end'], [236, 'end'], [70, 'end'], [69, 'end'], [62, 'end'], [68, 'end'], [64, 'end'], [67, 'end'], [60, 'end'], [59, 'end'], [57, 'end'], [55, 'end'], [35, 'end'], [33, 'end'], [234, 'end'], [232, 'end'], [197, 'end'], [195, 'end'], [194, 'end'], [192, 'end'], [190, 'end'], [189, 'end'], [188, 'end'], [186, 'end'], [185, 'end'], [182, 'end'], [201, 'end'], [200, 'end'], [137, 'end'], [135, 'end'], [107, 'end'], [133, 'end'], [132, 'end'], [30, 'end'], [28, 'end'], [26, 'end'], [2, 'end'], [175, 'end'], [173, 'end'], [145, 'end'], [143, 'end'], [142, 'end'], [140, 'end'], [170, 'end'], [105, 'end'], [103, 'end'], [75, 'end'], [73, 'end'], [96, 'end'],
 [180, 'end'], [179, 'end'], [178, 'end'], 'end'];
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


