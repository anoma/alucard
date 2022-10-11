var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [14, 14, 14, 4, 5, 13, 7, 8, 13, 10, 11, 13, 13, 14, 15, 16, 17, 19, 19, 20, , 27, 27, 27, 25, 27, 27, 28, , 50, 31, 32, 40, 34, 35, 40, 40, 38, 39, 40, 46, 42, 43, 46, 46, 46, 47, 48, 49, 50,
 51, , 54, 54, 55, 156, 57, 58, 59, 132, 61, 62, 63, 132, 65, 66, 67, 132, 69, 117, 115, 72, 76, 76, 76, 76, 78, 78, 106, 80, 93, 82, 89, 88, 85, 87, 87, 88, 89, 92, 91, 92, 93, 106, 95, 102, 98, 98, 101, 100,
 101, 102, 105, 104, 105, 106, 114, 111, 111, 111, 111, 114, 114, 114, 115, 116, 117, 127, 119, 126, 126, 122, 126, 126, 125, 126, 127, 132, 129, 130, 131, 132, 142, 134, 135, 142, 137, 139, 139, 140, 141, 142, 156, 145, 145, 146, 156, 149, 149, 150,
 156, 154, 153, 154, 155, 156, 157, , 159, 166, 166, 162, 166, 164, 166, 166, 183, 169, 169, 175, 171, 175, 173, 174, 175, 183, 177, 178, 182, 180, 181, 182, 183, 188, 187, 186, 187, 188, 189, , 193, 192, 193, 215, 195, 196, 205, 199, 199, 205,
 204, 204, 204, 204, 205, 214, 207, 208, 214, 213, 213, 213, 213, 214, 215, 216, , 221, 219, 221, 221, 222, , 224, 225, 252, 227, 229, 229, 230, 231, 252, 233, 251, 249, 241, 238, 238, 240, 240, 241, 242, 243, 247, 245, 247, 247, 248, 249, 250,
 251, 252, 253, , 255, 257, 257, 268, 259, 260, 268, 264, 263, 264, 265, 267, 267, 268, 269, , 'end'];

var FunctionNotes = [216, 253, 242, 20, 28, 189, 157, 55, 155, 150, 146, 51, 222, 269, 'end'];

var CodeTags = [[8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'],
 [6, 'end'], [6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'],
 [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'],
 [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'],
 [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], [8, 6, 'end'], 'end'];

var SourceCodeNotes = [[216, 'end'], [215, 'end'], [193, 'end'], [214, 'end'], [208, 'end'], [213, 'end'], [196, 'end'], [204, 'end'], [199, 'end'], [198, 'end'], [253, 'end'], [252, 'end'], [231, 'end'], [225, 'end'], [251, 'end'], [249, 'end'], [248, 'end'], [247, 'end'], [243, 'end'], [241, 'end'], [240, 'end'], [238, 'end'], [245, 'end'], [20, 'end'], [19, 'end'], [16, 'end'], [15, 'end'], [14, 'end'], [28, 'end'], [25, 'end'], [24, 'end'], [189, 'end'], [188, 'end'], [187, 'end'], [183, 'end'], [182, 'end'], [178, 'end'], [181, 'end'], [180, 'end'], [166, 'end'], [174, 'end'], [171, 'end'], [170, 'end'], [169, 'end'], [168, 'end'], [157, 'end'], [156, 'end'], [55, 'end'], [54, 'end'], [155, 'end'],
 [154, 'end'], [146, 'end'], [145, 'end'], [150, 'end'], [149, 'end'], [142, 'end'], [135, 'end'], [141, 'end'], [132, 'end'], [117, 'end'], [115, 'end'], [114, 'end'], [111, 'end'], [106, 'end'], [93, 'end'], [80, 'end'], [92, 'end'], [89, 'end'], [88, 'end'], [87, 'end'], [85, 'end'], [82, 'end'], [91, 'end'], [105, 'end'], [104, 'end'], [102, 'end'], [101, 'end'], [98, 'end'], [100, 'end'], [95, 'end'], [78, 'end'], [76, 'end'], [72, 'end'], [51, 'end'], [50, 'end'], [48, 'end'], [47, 'end'], [46, 'end'], [43, 'end'], [42, 'end'], [222, 'end'], [219, 'end'], [218, 'end'], [269, 'end'], [268, 'end'], [257, 'end'], [260, 'end'], [267, 'end'], [265, 'end'], 'end'];
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


