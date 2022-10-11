var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [3, 2, 3, 9, 9, 7, 7, 9, 9, 10, 11, , 13, 15, 15, 20, 20, 18, 20, 20, 21, 22, , 29, 29, 29, 29, 29, 29, 30, 31, , 33, 36, 36, 36, 83, 38, 39, 40, 43, 42, 43, 83, 63, 63, 47, 48, 49, 62,
 62, 52, 53, 58, 55, 56, 57, 58, 59, 62, 61, 62, 63, 64, 68, 67, 67, 68, 83, 75, 75, 74, 74, 74, 75, 83, 77, 82, 79, 80, 81, 82, 83, 84, , 88, 87, 88, 93, 90, 93, 93, 93, 94, 95, , 97, 104, 99, 101,
 101, 104, 104, 104, 105, 106, , 115, 115, 115, 115, 115, 115, 115, 115, 116, 117, , 119, 122, 121, 122, 123, , 127, 126, 127, 134, 134, 132, 132, 132, 134, 134, 135, 136, , 138, 140, 140, 143, 143, 143, 144, 145, , 156, 150, 149, 150,
 156, 154, 154, 154, 156, 156, 157, 158, , 168, 168, 163, 163, 168, 167, 166, 167, 168, 169, 170, , 179, 173, 179, 179, 176, 178, 178, 179, 180, 181, , 191, 185, 185, 191, 191, 190, 189, 190, 191, 192, 193, , 195, 197, 197, 202, 202, 200,
 202, 202, 203, 204, , 213, 213, 213, 213, 213, 213, 213, 213, 214, 215, , 222, 218, 220, 220, 222, 222, 223, 224, , 233, 229, 228, 229, 233, 233, 232, 233, 234, 235, , 244, 240, 239, 240, 244, 242, 244, 244, 245, 246, , 248, 255, 255,
 255, 254, 253, 254, 255, 256, 257, , 259, 260, 261, , 272, 266, 266, 266, 272, 272, 269, 271, 271, 272, 273, 274, , 283, 279, 278, 279, 283, 283, 282, 283, 284, 285, , 289, 288, 289, 294, 291, 294, 294, 294, 295, 296, , 299, 299, 306,
 306, 306, 303, 305, 305, 306, 307, 308, , 315, 315, 314, 313, 314, 315, 316, 317, , 321, 321, 321, 328, 328, 328, 325, 327, 327, 328, 329, 330, , 337, 333, 335, 335, 337, 337, 338, 339, , 414, 404, 373, 344, 345, 346, 361, 348, 361, 350,
 358, 355, 353, 354, 355, 356, 357, 358, 359, 361, 361, 366, 363, 364, 366, 366, 370, 370, 370, 370, 373, 373, 373, 374, 404, 404, 385, 384, 379, 383, 382, 382, 383, 384, 385, 388, 387, 388, 390, 390, 395, 392, 394, 394, 395, 397, 397, 403, 400, 400,
 401, 403, 403, 404, 410, 406, 408, 408, 410, 410, 413, 412, 413, 414, 418, 416, 417, 418, 420, 420, 421, , 428, 428, 425, 427, 427, 428, 429, 430, , 432, 434, 434, 439, 439, 437, 439, 439, 440, 441, , 445, 444, 445, 450, 450, 450, 449, 450,
 451, 452, , 461, 461, 461, 461, 461, 461, 461, 461, 462, 463, , 465, 467, 467, 473, 470, 470, 473, 473, 473, 474, 475, , 485, 478, 480, 480, 485, 485, 484, 484, 485, 486, 487, , 496, 490, 496, 492, 494, 494, 496, 496, 497, 498, , 505,
 501, 503, 503, 505, 505, 506, 507, , 509, 511, 511, 516, 513, 516, 516, 516, 517, 518, , 527, 527, 527, 527, 527, 527, 527, 527, 528, 529, , 538, 538, 533, 535, 535, 538, 537, 538, 539, 540, , 542, 549, 549, 547, 546, 547, 549, 549, 550,
 551, , 558, 558, 557, 556, 557, 558, 559, 560, , 567, 567, 566, 565, 566, 567, 568, 569, , 576, 576, 576, 576, 576, 576, 577, 578, , 588, 582, 582, 588, 584, 586, 586, 588, 588, 589, 590, , 599, 599, 594, 596, 596, 599, 598, 599, 600,
 601, , 608, 608, 608, 608, 608, 608, 609, 610, , 614, 613, 614, 617, 617, 617, 618, 619, , 'end'];

var FunctionNotes = [261, 123, 529, 117, 610, 31, 95, 204, 274, 330, 224, 590, 158, 136, 475, 441, 170, 560, 430, 285, 235, 106, 22, 339, 181, 487, 11, 308, 246, 257, 507, 619, 145, 569, 601, 215, 498, 421, 317, 518, 452, 296, 84, 193, 540, 463, 578, 551, 'end'];

var CodeTags = [['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], [6, 4, 'end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], [9, 5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[261, 'end'], [260, 'end'], [259, 'end'], [123, 'end'], [122, 'end'], [529, 'end'], [117, 'end'], [610, 'end'], [31, 'end'], [95, 'end'], [204, 'end'], [274, 'end'], [330, 'end'], [224, 'end'], [590, 'end'], [158, 'end'], [136, 'end'], [475, 'end'], [441, 'end'], [170, 'end'], [560, 'end'], [430, 'end'], [285, 'end'], [235, 'end'], [106, 'end'], [22, 'end'], [339, 'end'], [181, 'end'], [487, 'end'], [11, 'end'], [308, 'end'], [246, 'end'], [257, 'end'], [507, 'end'], [619, 'end'], [145, 'end'], [569, 'end'], [601, 'end'], [215, 'end'], [498, 'end'], [421, 'end'], [317, 'end'], [518, 'end'], [452, 'end'], [296, 'end'], [84, 'end'], [75, 'end'], [193, 'end'], [540, 'end'], [463, 'end'],
 [578, 'end'], [551, 'end'], 'end'];
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


