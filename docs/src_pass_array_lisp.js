var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [1, 136, 4, 4, 136, 7, 7, 34, 9, 10, 13, 12, 13, 22, 15, 18, 18, 18, 19, 21, 21, 22, 23, 34, 29, 29, 29, 29, 29, 30, 33, 32, 33, 34, 135, 36, 130, 39, 39, 43, 42, 42, 43, 44, 115, 46, 112, 51, 49, 50,
 51, 53, 53, 60, 55, 59, 57, 58, 59, 60, 61, 108, 64, 64, 108, 74, 73, 68, 73, 70, 71, 72, 73, 74, 75, 107, 78, 78, 107, 84, 84, 84, 84, 84, 85, 88, 87, 88, 106, 90, 93, 92, 93, 101, 98, 96, 97, 98, 100, 100,
 101, 102, 106, 105, 105, 106, 107, 108, 112, 111, 111, 112, 115, 114, 115, 128, 118, 118, 124, 121, 121, 124, 123, 124, 125, 128, 127, 128, 129, 130, 131, 135, 134, 134, 135, 136, 137, 138, 153, 140, 142, 142, 143, 153, 151, 149, 147, 149, 149, 150,
 151, 152, 153, 154, , 156, 157, 158, 161, 160, 161, 180, 177, 165, 165, 177, 167, 168, 176, 170, 171, 176, 176, 174, 176, 176, 177, 180, 179, 180, 181, , 183, , 185, 186, 189, 188, 189, 208, 193, 193, 193, 204, 202, 196, 197, 202, 202, 200,
 201, 202, 204, 204, 208, 206, 208, 208, 209, , 225, 212, 215, 214, 215, 220, 219, 219, 219, 220, 221, 222, 223, 224, 225, 226, 231, 229, 229, 230, 231, 233, 233, 248, 235, 238, 237, 238, 244, 241, 241, 243, 243, 244, 245, 246, 247, 248, 249, ,
 254, 252, 254, 254, 255, , 'end'];

var FunctionNotes = [249, 245, 223, 154, 143, 152, 209, 181, 183, 255, 'end'];

var CodeTags = [[6, 'end'], [6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'],
 [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [6, 'end'], [6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [6, 'end'], [6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'],
 [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'],
 [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[249, 'end'], [248, 'end'], [246, 'end'], [244, 'end'], [238, 'end'], [243, 'end'], [241, 'end'], [154, 'end'], [153, 'end'], [143, 'end'], [142, 'end'], [140, 'end'], [152, 'end'], [151, 'end'], [150, 'end'], [149, 'end'], [138, 'end'], [0, 'end'], [130, 'end'], [36, 'end'], [126, 'end'], [113, 'end'], [45, 'end'], [60, 'end'], [59, 'end'], [57, 'end'], [53, 'end'], [51, 'end'], [50, 'end'], [74, 'end'], [73, 'end'], [72, 'end'], [70, 'end'], [101, 'end'], [93, 'end'], [100, 'end'], [98, 'end'], [97, 'end'], [22, 'end'], [13, 'end'], [9, 'end'], [21, 'end'], [19, 'end'], [18, 'end'], [209, 'end'], [208, 'end'], [206, 'end'], [189, 'end'], [185, 'end'], [207, 'end'],
 [204, 'end'], [202, 'end'], [200, 'end'], [197, 'end'], [196, 'end'], [193, 'end'], [190, 'end'], [181, 'end'], [180, 'end'], [179, 'end'], [161, 'end'], [157, 'end'], [156, 'end'], [177, 'end'], [176, 'end'], [170, 'end'], [167, 'end'], [165, 'end'], [164, 'end'], [183, 'end'], [255, 'end'], [252, 'end'], [251, 'end'], 'end'];
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


