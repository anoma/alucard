var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [5, 5, 5, 4, 5, 6, 16, 15, 10, 10, 11, 15, 14, 14, 15, 16, 17, 18, , 20, 21, , 23, 29, 29, 29, 29, 29, 29, 30, 31, 32, , 40, 40, 40, 40, 38, 40, 40, 41, 42, , 48, 48, 46, 48, 48, 49, 50,
 , 54, 54, 54, 64, 56, 61, 58, 61, 61, 61, 62, 63, 64, 65, 66, , 75, 75, 70, 75, 72, 75, 74, 75, 76, 77, 82, 81, 81, 81, 82, 83, 84, , 87, 87, 88, , 90, 93, 93, 93, 95, 95, 96, , 104, 104, 100,
 104, 104, 104, 104, 105, 106, , 110, 110, 110, 120, 112, 117, 117, 117, 116, 117, 118, 119, 120, 121, 122, , 124, 127, 127, 127, 129, 129, 130, , 140, 140, 140, 135, 140, 140, 140, 140, 140, 141, 142, 143, , 147, 147, 147, 157, 154, 150,
 154, 154, 153, 154, 155, 156, 157, 158, 159, , 161, 164, 164, 164, 166, 166, 167, , 169, 172, 172, 172, 174, 174, 175, , 182, 181, 179, 181, 181, 182, 183, , 185, 187, 187, 188, 189, , 197, 197, 193, 197, 195, 196, 197, 198, 199, 200,
 205, 204, 204, 204, 205, 206, 207, , 213, 210, 213, 213, 213, 214, 215, 216, , 218, 223, 220, 223, 223, 223, 224, 225, 230, 229, 229, 229, 230, 231, 232, , 236, 236, 236, 272, 240, 240, 240, 266, 242, 244, 244, 246, 246, 247, 266, 265, 250,
 251, 252, 253, 254, 255, 256, 261, 258, 261, 260, 261, 264, 263, 264, 265, 266, 269, 268, 269, 270, 271, 272, 273, 274, , 277, 277, 278, , 280, 292, 282, 288, 288, 288, 288, 288, 288, 289, 292, 291, 292, , 294, 304, 302, 302, 302, 299, 302,
 302, 302, 303, 304, , 314, 308, 308, 314, 314, 312, 312, 314, 314, 317, 316, 317, 318, , 321, 321, 381, 323, 381, 331, 331, 331, 328, 329, 330, 331, 381, 359, 359, 335, 336, 357, 357, 339, 340, 341, 356, 346, 344, 345, 346, 347, 348, 351, 350,
 351, 352, 356, 356, 355, 356, 357, 359, 359, 368, 361, 368, 368, 368, 368, 368, 368, 368, 369, 379, 378, 373, 373, 378, 376, 376, 377, 378, 379, 380, 381, 382, , 389, 385, 389, 387, 389, 389, 390, 391, 392, , 399, 395, 399, 399, 398, 399, 400,
 401, 406, 405, 405, 405, 406, 407, 408, , 416, 416, 412, 416, 416, 416, 416, 417, 418, 419, , 427, 427, 427, 424, 427, 427, 427, 428, 433, 430, 433, 432, 433, , 440, 439, 437, 439, 439, 440, 441, , 443, 449, 449, 449, 449, 449, 449, 450,
 451, , 458, 454, 458, 458, 457, 458, 459, 460, 465, 464, 464, 464, 465, 466, 467, , 471, 471, 471, 507, 473, 504, 477, 477, 477, 503, 481, 480, 481, 483, 483, 484, 503, 502, 487, 488, 489, 490, 491, 492, 493, 498, 495, 498, 497, 498, 501, 500,
 501, 502, 503, 504, 505, 506, 507, 508, 509, , 513, 513, 513, 524, 520, 520, 517, 520, 519, 520, 521, 522, 523, 524, 525, 526, , 533, 532, 532, 531, 532, 533, 534, , 570, 537, 539, 539, 541, 541, 542, 543, 570, 545, 546, 568, 565, 549, 557,
 551, 552, 554, 554, 555, 556, 557, 558, 565, 560, 565, 562, 563, 564, 565, 568, 567, 568, 570, 570, 571, , 578, 577, 577, 576, 577, 578, 579, , 583, 583, 583, 619, 587, 587, 587, 613, 605, 590, 604, 592, 603, 594, 603, 596, 597, 598, 599, 600,
 601, 602, 603, 604, 605, 613, 609, 608, 609, 611, 611, 612, 613, 616, 615, 616, 617, 618, 619, 620, 621, , 623, 626, 626, 626, 628, 628, 629, , 632, 632, 633, , 'end'];

var FunctionNotes = [579, 167, 441, 130, 183, 96, 175, 534, 629, 318, 21, 88, 278, 633, 621, 617, 50, 526, 522, 304, 207, 199, 382, 329, 571, 542, 189, 159, 155, 433, 467, 459, 451, 392, 292, 509, 505, 18, 274, 270, 216, 408, 400, 419, 122, 118, 32, 84, 76, 143,
 66, 62, 42, 232, 224, 106, 'end'];

var CodeTags = [[13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], ['end'], ['end'], ['end'], ['end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], [13, 12, 8, 7, 6, 5, 4, 1, 'end'], ['end'], ['end'], ['end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [13, 8, 6, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'],
 [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'],
 [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [7, 2, 'end'], [7, 2, 'end'], [7, 2, 'end'], [7, 2, 'end'], [7, 2, 'end'], [7, 2, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], [13, 12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], [13, 12, 8, 7, 6, 4, 'end'], ['end'], ['end'], [13, 12, 8, 7, 6, 4, 'end'], ['end'], ['end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'],
 [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [13, 12, 9, 8, 7, 6, 5, 4, 2, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'],
 [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], [12, 7, 6, 5, 2, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [13, 12, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], [12, 7, 6, 5, 4, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 7, 6, 5, 4, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'],
 [13, 12, 8, 7, 6, 'end'], [13, 12, 8, 7, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], [5, 'end'], ['end'], ['end'], ['end'], ['end'], 'end'];

var SourceCodeNotes = [[579, 'end'], [572, 'end'], [167, 'end'], [165, 'end'], [441, 'end'], [434, 'end'], [130, 'end'], [128, 'end'], [183, 'end'], [176, 'end'], [96, 'end'], [94, 'end'], [175, 'end'], [173, 'end'], [534, 'end'], [527, 'end'], [629, 'end'], [627, 'end'], [318, 'end'], [317, 'end'], [314, 'end'], [21, 'end'], [20, 'end'], [19, 'end'], [88, 'end'], [87, 'end'], [85, 'end'], [278, 'end'], [277, 'end'], [276, 'end'], [633, 'end'], [632, 'end'], [631, 'end'], [621, 'end'], [619, 'end'], [616, 'end'], [584, 'end'], [615, 'end'], [50, 'end'], [49, 'end'], [526, 'end'], [524, 'end'], [521, 'end'], [520, 'end'], [514, 'end'], [304, 'end'], [294, 'end'], [293, 'end'], [303, 'end'], [207, 'end'],
 [205, 'end'], [198, 'end'], [197, 'end'], [191, 'end'], [196, 'end'], [382, 'end'], [381, 'end'], [323, 'end'], [331, 'end'], [330, 'end'], [328, 'end'], [324, 'end'], [321, 'end'], [319, 'end'], [380, 'end'], [369, 'end'], [359, 'end'], [333, 'end'], [571, 'end'], [570, 'end'], [543, 'end'], [541, 'end'], [539, 'end'], [537, 'end'], [567, 'end'], [189, 'end'], [188, 'end'], [187, 'end'], [185, 'end'], [159, 'end'], [157, 'end'], [154, 'end'], [151, 'end'], [150, 'end'], [153, 'end'], [433, 'end'], [432, 'end'], [431, 'end'], [430, 'end'], [429, 'end'], [428, 'end'], [467, 'end'], [465, 'end'], [458, 'end'], [452, 'end'], [454, 'end'], [457, 'end'], [451, 'end'], [450, 'end'], [392, 'end'],
 [390, 'end'], [389, 'end'], [388, 'end'], [292, 'end'], [291, 'end'], [290, 'end'], [280, 'end'], [279, 'end'], [289, 'end'], [509, 'end'], [507, 'end'], [504, 'end'], [475, 'end'], [473, 'end'], [18, 'end'], [17, 'end'], [6, 'end'], [274, 'end'], [272, 'end'], [269, 'end'], [238, 'end'], [268, 'end'], [216, 'end'], [215, 'end'], [214, 'end'], [408, 'end'], [406, 'end'], [399, 'end'], [393, 'end'], [398, 'end'], [395, 'end'], [419, 'end'], [418, 'end'], [417, 'end'], [122, 'end'], [120, 'end'], [117, 'end'], [114, 'end'], [116, 'end'], [112, 'end'], [32, 'end'], [31, 'end'], [30, 'end'], [84, 'end'], [82, 'end'], [75, 'end'], [68, 'end'], [72, 'end'], [70, 'end'], [74, 'end'],
 [143, 'end'], [142, 'end'], [141, 'end'], [66, 'end'], [64, 'end'], [61, 'end'], [60, 'end'], [56, 'end'], [58, 'end'], [42, 'end'], [41, 'end'], [232, 'end'], [230, 'end'], [223, 'end'], [222, 'end'], [220, 'end'], [218, 'end'], [106, 'end'], [105, 'end'], 'end'];
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


