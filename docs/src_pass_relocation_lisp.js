var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [3, 3, 3, 4, 5, , 8, 8, 9, , 12, 12, 13, 14, , 16, 19, 18, 19, 20, 21, 22, , 26, 26, 26, 27, , 98, 98, 53, 32, 33, 34, 49, 36, 44, 41, 39, 40, 41, 42, 43, 44, 45, 49, 49, 48, 49, 53,
 51, 52, 53, 98, 83, 60, 60, 60, 60, 60, 61, 64, 63, 64, 73, 67, 67, 73, 69, 70, 71, 72, 73, 80, 76, 76, 80, 78, 79, 80, 81, 82, 83, 95, 87, 87, 87, 95, 89, 90, 91, 94, 93, 94, 95, 96, 97, 98, 99, ,
 102, 102, 105, 105, 105, 106, , 110, 109, 110, 111, 116, 116, 114, 116, 116, 117, , 120, 120, 123, 123, 123, 124, , 129, 128, 128, 129, 130, , 176, 171, 171, 136, 136, 164, 139, 139, 161, 141, 142, 143, 149, 145, 146, 149, 148, 149, 150,
 161, 156, 156, 156, 156, 156, 157, 160, 159, 160, 161, 164, 163, 164, 165, 166, 167, 168, 171, 171, 171, 175, 173, 174, 175, 176, , 180, 180, 180, 181, , 184, 184, 186, 186, 187, , 189, 193, 193, 193, 193, 553, 195, 546, 197, 546, 200, 200,
 210, 203, 203, 210, 206, 206, 210, 209, 209, 210, 211, 545, 213, 545, 215, 248, 217, 226, 219, 225, 225, 225, 223, 224, 225, 226, 227, 237, 237, 235, 231, 235, 233, 234, 235, 237, 237, 246, 239, 243, 241, 242, 243, 246, 245, 246, 248, 248, 256, 250,
 251, 252, 256, 254, 255, 256, 257, 520, 260, 260, 520, 262, 264, 264, 321, 319, 267, 319, 269, 276, 271, 272, 276, 274, 275, 276, 302, 280, 279, 280, 302, 282, 284, 284, 301, 301, 301, 288, 300, 290, 291, 300, 293, 297, 295, 296, 297, 298, 299, 300,
 301, 302, 318, 314, 314, 306, 307, 311, 309, 310, 311, 312, 314, 314, 318, 316, 317, 318, 319, 321, 321, 341, 332, 332, 325, 326, 332, 331, 331, 331, 331, 332, 341, 334, 335, 336, 337, 340, 339, 340, 341, 342, 519, 345, 345, 519, 351, 351, 351, 351,
 351, 352, 355, 354, 355, 518, 431, 358, 362, 360, 361, 362, 376, 366, 365, 366, 367, 370, 369, 370, 376, 375, 374, 374, 375, 376, 377, 378, 431, 431, 381, 398, 398, 384, 385, 386, 398, 388, 389, 391, 391, 392, 393, 396, 395, 396, 397, 398, 430, 400,
 401, 430, 429, 429, 405, 422, 407, 415, 409, 410, 412, 412, 413, 414, 415, 416, 422, 422, 419, 420, 421, 422, 427, 427, 425, 426, 427, 429, 429, 430, 431, 513, 448, 448, 437, 436, 437, 448, 439, 440, 447, 447, 445, 444, 445, 446, 447, 448, 513, 450,
 452, 452, 457, 456, 455, 456, 457, 464, 459, 461, 461, 464, 464, 464, 487, 466, 467, 470, 469, 470, 487, 479, 473, 478, 475, 478, 478, 478, 479, 480, 486, 484, 483, 484, 485, 486, 487, 499, 489, 494, 491, 492, 493, 494, 499, 498, 498, 498, 499, 500,
 501, 509, 509, 509, 509, 508, 508, 508, 509, 513, 511, 512, 513, 514, 518, 517, 517, 518, 519, 520, 544, 523, 523, 544, 525, 530, 527, 528, 529, 530, 542, 541, 533, 534, 539, 536, 539, 538, 539, 541, 541, 542, 543, 544, 545, 546, 547, 550, 549, 550,
 552, 552, 553, 554, , 556, 559, 558, 559, 574, 573, 562, 566, 564, 566, 566, 573, 568, 571, 570, 571, 573, 573, 574, 575, , 605, 604, 604, 580, 581, 602, 602, 584, 592, 589, 587, 588, 589, 590, 591, 592, 593, 601, 595, 601, 601, 598, 599, 600,
 601, 602, 604, 604, 605, 607, 607, 608, 643, 610, 643, 640, 639, 639, 632, 616, 617, 618, 632, 620, 632, 622, 630, 624, 625, 627, 627, 628, 629, 630, 631, 632, 637, 634, 635, 637, 637, 639, 639, 640, 642, 642, 643, 644, , 702, 702, 650, 650, 650,
 675, 661, 660, 654, 660, 656, 659, 658, 659, 660, 661, 667, 663, 666, 665, 666, 667, 675, 669, 674, 671, 672, 673, 674, 675, 676, 677, 702, 701, 683, 681, 682, 683, 684, 685, 688, 687, 688, 689, 697, 691, 692, 693, 697, 697, 696, 697, 701, 699, 700,
 701, 702, 703, , 'end'];

var FunctionNotes = [14, 9, 187, 124, 130, 106, 27, 554, 500, 377, 176, 167, 22, 99, 96, 575, 181, 117, 703, 676, 5, 644, 'end'];

var CodeTags = [[6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 'end'], [6, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], [6, 1, 'end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'],
 [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], [6, 1, 'end'], 'end'];

var SourceCodeNotes = [[187, 130, 124, 106, 27, 14, 9, 'end'], [554, 'end'], [553, 'end'], [193, 'end'], [189, 'end'], [552, 'end'], [550, 'end'], [542, 'end'], [530, 'end'], [528, 'end'], [541, 'end'], [539, 'end'], [256, 'end'], [252, 'end'], [251, 'end'], [255, 'end'], [248, 'end'], [215, 'end'], [246, 'end'], [243, 'end'], [245, 'end'], [237, 'end'], [235, 'end'], [227, 'end'], [226, 'end'], [225, 'end'], [341, 'end'], [340, 'end'], [336, 'end'], [335, 'end'], [332, 'end'], [326, 'end'], [323, 'end'], [321, 'end'], [320, 'end'], [264, 'end'], [263, 'end'], [265, 'end'], [267, 'end'], [266, 'end'], [317, 'end'], [316, 'end'], [302, 'end'], [276, 'end'], [274, 'end'], [280, 'end'], [301, 'end'], [284, 'end'], [300, 'end'], [299, 'end'],
 [297, 'end'], [314, 'end'], [312, 'end'], [311, 'end'], [309, 'end'], [513, 'end'], [512, 'end'], [509, 'end'], [501, 'end'], [499, 'end'], [487, 'end'], [470, 'end'], [486, 'end'], [479, 'end'], [478, 'end'], [484, 'end'], [464, 'end'], [461, 'end'], [457, 'end'], [452, 'end'], [456, 'end'], [508, 'end'], [431, 'end'], [378, 'end'], [376, 'end'], [362, 'end'], [370, 'end'], [366, 'end'], [375, 'end'], [374, 'end'], [429, 'end'], [402, 'end'], [448, 'end'], [447, 'end'], [445, 'end'], [437, 'end'], [176, 'end'], [131, 'end'], [175, 'end'], [174, 'end'], [171, 'end'], [168, 'end'], [166, 'end'], [149, 'end'], [145, 'end'], [143, 'end'], [141, 'end'], [22, 'end'], [21, 'end'], [20, 'end'],
 [19, 'end'], [99, 'end'], [98, 'end'], [97, 'end'], [95, 'end'], [83, 'end'], [82, 'end'], [78, 'end'], [71, 'end'], [69, 'end'], [575, 'end'], [574, 'end'], [559, 'end'], [573, 'end'], [571, 'end'], [570, 'end'], [568, 'end'], [566, 'end'], [181, 'end'], [180, 'end'], [178, 'end'], [117, 'end'], [116, 'end'], [110, 'end'], [703, 'end'], [702, 'end'], [677, 'end'], [675, 'end'], [667, 'end'], [666, 'end'], [661, 'end'], [660, 'end'], [654, 'end'], [659, 'end'], [5, 'end'], [4, 'end'], [3, 'end'], [644, 'end'], [643, 'end'], [640, 605, 'end'], [611, 576, 'end'], [639, 604, 'end'], [612, 578, 'end'], [606, 'end'], 'end'];
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


