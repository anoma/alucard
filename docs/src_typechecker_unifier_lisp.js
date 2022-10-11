var NO_DATA = 1, NOT_COVERED = 1, ALL_COVERED = 2, PARTLY_COVERED = 3;


var CodeParents = [1, 6, 3, 6, 5, 6, 120, 45, 9, 10, 11, 43, 14, 14, 15, 16, 25, 18, 21, 20, 21, 22, 23, 24, 25, 26, 29, 28, 29, 30, 38, 32, 33, 34, 38, 38, 37, 38, 43, 40, 41, 42, 43, 45, 45, 120, 47, 48, 51, 50,
 51, 75, 56, 56, 56, 56, 74, 58, 63, 60, 63, 62, 63, 72, 71, 71, 70, 70, 70, 70, 71, 72, 74, 74, 75, 76, 112, 78, 79, 80, 105, 88, 86, 84, 85, 86, 88, 88, 105, 94, 94, 94, 94, 94, 105, 96, 97, 100, 99, 100,
 101, 102, 104, 104, 105, 106, 112, 111, 109, 110, 111, 112, 120, 114, 115, 119, 119, 119, 119, 120, 121, , 274, 125, 125, 126, 274, 274, 274, 130, 133, 132, 133, 134, 274, 136, 139, 138, 139, 152, 152, 145, 145, 145, 145, 152, 147, 150, 149, 150,
 151, 152, 153, 274, 156, 156, 271, 159, 159, 185, 161, 185, 164, 164, 184, 172, 172, 172, 172, 172, 172, 172, 173, 176, 175, 176, 181, 178, 181, 180, 181, 184, 183, 184, 185, 186, 187, 188, 271, 190, 234, 192, 219, 194, 219, 197, 197, 218, 199, 215,
 202, 202, 215, 210, 210, 210, 210, 210, 210, 210, 211, 214, 213, 214, 215, 218, 217, 218, 219, 220, 221, 222, 234, 224, 233, 231, 231, 231, 231, 231, 231, 232, 233, 234, 270, 236, 263, 238, 259, 240, 250, 248, 248, 248, 248, 248, 248, 248, 249, 250,
 255, 252, 255, 254, 255, 259, 258, 258, 259, 263, 262, 262, 263, 264, 265, 266, 270, 269, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, , 280, 281, 284, 283, 284, 313, 286, 292, 289, 289, 290, 291, 292, 313, 313, 295, 313, 297, 298, 301, 300,
 301, 313, 313, 313, 313, 313, 312, 308, 309, 312, 311, 312, 313, 314, 315, 316, , 320, 320, 320, 321, , 430, 324, 327, 326, 327, 332, 331, 331, 331, 332, 333, 334, 429, 337, 337, 338, 427, 342, 341, 342, 426, 363, 345, 346, 347, 363, 350, 350,
 351, 356, 353, 354, 355, 356, 363, 358, 361, 360, 361, 362, 363, 367, 367, 366, 367, 368, 369, 370, 371, 402, 396, 374, 375, 376, 393, 378, 381, 380, 381, 382, 393, 385, 385, 386, 391, 388, 389, 390, 391, 393, 393, 396, 395, 396, 397, 398, 399, 402,
 401, 402, 425, 404, 418, 406, 407, 408, 418, 410, 415, 412, 413, 414, 415, 416, 418, 418, 420, 420, 421, 422, 425, 424, 425, 426, 427, 428, 429, 430, 502, 433, 433, 498, 436, 436, 488, 442, 442, 442, 442, 442, 443, 446, 445, 446, 478, 448, 449, 478,
 451, 452, 453, 465, 455, 465, 465, 458, 463, 462, 461, 462, 463, 464, 465, 467, 467, 468, 469, 476, 472, 472, 476, 475, 475, 476, 477, 478, 488, 480, 485, 482, 485, 484, 485, 486, 487, 488, 489, 490, 496, 492, 495, 494, 495, 496, 498, 498, 499, 500,
 501, 502, 503, , 505, 506, 634, 509, 509, 513, 512, 512, 513, 514, 621, 520, 520, 520, 520, 520, 521, 524, 523, 524, 621, 526, 527, 619, 530, 530, 544, 536, 536, 536, 536, 536, 537, 540, 539, 540, 544, 542, 543, 544, 552, 546, 547, 548, 552, 551,
 551, 552, 553, 554, 619, 556, 557, 616, 560, 560, 604, 562, 570, 568, 568, 568, 568, 568, 569, 570, 604, 572, 602, 574, 601, 592, 577, 591, 591, 580, 581, 582, 591, 586, 585, 586, 589, 588, 589, 590, 591, 592, 593, 594, 601, 596, 600, 598, 599, 600,
 601, 602, 603, 604, 612, 612, 610, 610, 610, 610, 611, 612, 616, 615, 615, 616, 617, 618, 619, 620, 621, 629, 623, 625, 625, 626, 629, 628, 629, 630, 631, 632, 633, 634, 635, 636, 637, 640, 639, 640, 641, 642, 919, 649, 649, 646, 649, 649, 649, 650,
 651, 652, 653, 654, 655, 919, 657, 658, 905, 664, 664, 664, 664, 664, 665, 891, 667, 672, 669, 672, 671, 672, 673, 891, 683, 678, 677, 678, 679, 683, 683, 683, 683, 693, 687, 686, 687, 688, 691, 690, 691, 693, 693, 694, 891, 696, 725, 704, 704, 704,
 704, 704, 704, 704, 705, 708, 707, 708, 719, 711, 711, 715, 714, 714, 715, 716, 719, 718, 719, 725, 721, 723, 723, 724, 725, 726, 727, 728, 783, 730, 732, 732, 733, 779, 735, 753, 737, 739, 739, 740, 753, 748, 748, 748, 748, 748, 748, 748, 749, 752,
 751, 752, 753, 764, 755, 764, 758, 758, 762, 761, 761, 762, 763, 764, 765, 766, 767, 779, 769, 778, 776, 776, 776, 776, 776, 776, 777, 778, 779, 783, 782, 782, 783, 784, 785, 891, 878, 878, 793, 793, 793, 793, 793, 794, 797, 796, 797, 877, 799, 877,
 801, 802, 803, 809, 808, 808, 808, 808, 809, 810, 874, 828, 827, 814, 827, 816, 817, 818, 827, 820, 822, 822, 825, 824, 825, 826, 827, 828, 829, 830, 874, 835, 835, 835, 835, 836, 839, 838, 839, 856, 843, 842, 843, 844, 856, 846, 847, 848, 854, 853,
 853, 853, 853, 854, 855, 856, 873, 858, 859, 860, 866, 865, 865, 865, 865, 866, 867, 873, 871, 870, 871, 872, 873, 874, 875, 876, 877, 878, 879, 880, 889, 882, 888, 884, 888, 886, 888, 888, 889, 890, 891, 892, 893, 894, 902, 896, 901, 898, 901, 900,
 901, 902, 903, 904, 905, 906, 907, 911, 910, 910, 911, 912, 916, 915, 915, 916, 917, 918, 919, 920, 927, 925, 925, 925, 925, 926, 927, 928, , 'end'];

var FunctionNotes = [928, 926, 121, 76, 106, 101, 503, 499, 334, 278, 321, 316, 290, 'end'];

var CodeTags = [[12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], [12, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'],
 ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], ['end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'],
 [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 'end'], [12, 8, 6, 'end'], [12, 8, 6, 'end'], 'end'];

var SourceCodeNotes = [[928, 'end'], [927, 'end'], [926, 'end'], [925, 'end'], [922, 'end'], [924, 'end'], [920, 'end'], [902, 'end'], [901, 'end'], [894, 'end'], [664, 'end'], [663, 'end'], [660, 'end'], [693, 'end'], [691, 'end'], [683, 'end'], [681, 'end'], [674, 'end'], [679, 'end'], [880, 'end'], [875, 'end'], [809, 'end'], [802, 'end'], [808, 'end'], [805, 'end'], [806, 'end'], [866, 'end'], [859, 'end'], [865, 'end'], [862, 'end'], [861, 'end'], [854, 'end'], [847, 'end'], [853, 'end'], [850, 'end'], [849, 'end'], [835, 'end'], [832, 'end'], [833, 'end'], [888, 'end'], [672, 'end'], [631, 'end'], [619, 'end'], [554, 'end'], [547, 'end'], [546, 'end'], [542, 'end'], [527, 'end'], [618, 'end'], [556, 'end'],
 [610, 'end'], [607, 'end'], [606, 'end'], [602, 'end'], [572, 'end'], [598, 'end'], [121, 'end'], [120, 'end'], [119, 'end'], [115, 'end'], [6, 'end'], [45, 'end'], [14, 'end'], [42, 'end'], [40, 'end'], [112, 'end'], [76, 'end'], [75, 'end'], [51, 'end'], [47, 'end'], [74, 'end'], [72, 'end'], [63, 'end'], [71, 'end'], [70, 'end'], [67, 'end'], [69, 'end'], [56, 'end'], [54, 'end'], [53, 'end'], [106, 'end'], [105, 'end'], [104, 'end'], [102, 'end'], [100, 'end'], [96, 'end'], [94, 'end'], [89, 'end'], [80, 'end'], [78, 'end'], [88, 'end'], [86, 'end'], [85, 'end'], [111, 'end'], [110, 'end'], [503, 'end'], [502, 'end'], [500, 'end'], [498, 'end'], [433, 'end'],
 [496, 'end'], [495, 'end'], [490, 'end'], [486, 'end'], [485, 'end'], [448, 'end'], [342, 'end'], [278, 'end'], [277, 'end'], [133, 'end'], [130, 'end'], [132, 'end'], [152, 'end'], [140, 'end'], [139, 'end'], [138, 'end'], [136, 'end'], [150, 'end'], [149, 'end'], [147, 'end'], [125, 'end'], [321, 'end'], [316, 'end'], [315, 'end'], [314, 'end'], [312, 'end'], [308, 'end'], [292, 'end'], [291, 'end'], [289, 'end'], [286, 'end'], [284, 'end'], [280, 'end'], [301, 'end'], [297, 'end'], 'end'];
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


