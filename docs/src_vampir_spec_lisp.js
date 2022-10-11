var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [1, 5, 5, 5, 5, 6, 7, , 12, 10, 12, 12, 14, 14, 15, , 17, 21, 21, 21, 21, 22, 23, , 30, 29, 29, 28, 29, 30, 31, , 36, 34, 36, 36, 38, 38, 39, , 47, 42, 47, 47, 47, 47, 47, 48, 49, ,
 57, 57, 57, 54, 57, 57, 57, 58, 59, , 61, 93, 65, 65, 65, 92, 68, 68, 87, 71, 71, 87, 74, 74, 87, 77, 77, 87, 80, 80, 87, 83, 83, 87, 86, 86, 87, 88, 89, 90, 92, 92, 93, 94, , 100, 100, 98, 100, 100,
 101, 102, , 107, 107, 106, 107, 109, 109, 110, , 120, 120, 120, 115, 120, 120, 120, 120, 120, 121, 122, , 132, 132, 132, 132, 128, 132, 132, 132, 132, 133, 134, , 141, 137, 140, 140, 140, 141, 142, , 150, 150, 150, 150, 150, 149, 150,
 151, 152, , 154, 157, 157, 157, 159, 159, 160, , 162, 165, 165, 165, 167, 167, 168, , 184, 172, 172, 176, 175, 175, 176, 177, 178, 179, 184, 183, 183, 183, 184, 187, 186, 187, 188, , 194, 194, 192, 194, 194, 195, 196, , 'end'];

var FunctionNotes = [15, 168, 31, 39, 142, 110, 160, 94, 89, 188, 178, 134, 196, 122, 59, 49, 152, 102, 23, 7, 'end'];

var CodeTags = [[6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'],
 [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'],
 [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'],
 [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [6, 5, 'end'], ['end'], ['end'], ['end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [6, 5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], 'end'];

var SourceCodeNotes = [[15, 'end'], [13, 'end'], [168, 'end'], [166, 'end'], [31, 'end'], [24, 'end'], [39, 'end'], [37, 'end'], [142, 'end'], [135, 'end'], [110, 'end'], [108, 'end'], [160, 'end'], [158, 'end'], [94, 'end'], [93, 'end'], [61, 'end'], [92, 'end'], [90, 'end'], [88, 'end'], [188, 'end'], [187, 'end'], [186, 'end'], [184, 'end'], [179, 'end'], [177, 'end'], [134, 'end'], [133, 'end'], [196, 'end'], [195, 'end'], [122, 'end'], [121, 'end'], [59, 'end'], [58, 'end'], [49, 'end'], [48, 'end'], [152, 'end'], [151, 'end'], [102, 'end'], [101, 'end'], [23, 'end'], [22, 'end'], [7, 'end'], [6, 'end'], 'end'];
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


