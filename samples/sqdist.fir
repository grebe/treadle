;buildInfoPackage: chisel3, version: 3.0-SNAPSHOT, scalaVersion: 2.11.11, sbtVersion: 0.13.15, builtAtString: 2017-08-25 22:51:12.887, builtAtMillis: 1503701472887
circuit sqdist : 
  module sqdist : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip in1 : Fixed<32><<16>>[10], flip in2 : Fixed<32><<16>>[10], out : Fixed<32><<16>>}
    
    clock is invalid
    reset is invalid
    io is invalid
    node _T_30 = sub(io.in2[0], io.in1[0]) @[sqdist.scala 19:51]
    node _T_31 = tail(_T_30, 1) @[sqdist.scala 19:51]
    node _T_32 = asFixedPoint(_T_31, 16) @[sqdist.scala 19:51]
    node _T_33 = sub(io.in2[0], io.in1[0]) @[sqdist.scala 19:61]
    node _T_34 = tail(_T_33, 1) @[sqdist.scala 19:61]
    node _T_35 = asFixedPoint(_T_34, 16) @[sqdist.scala 19:61]
    node _T_36 = mul(_T_32, _T_35) @[sqdist.scala 19:56]
    node _T_37 = sub(io.in2[1], io.in1[1]) @[sqdist.scala 19:51]
    node _T_38 = tail(_T_37, 1) @[sqdist.scala 19:51]
    node _T_39 = asFixedPoint(_T_38, 16) @[sqdist.scala 19:51]
    node _T_40 = sub(io.in2[1], io.in1[1]) @[sqdist.scala 19:61]
    node _T_41 = tail(_T_40, 1) @[sqdist.scala 19:61]
    node _T_42 = asFixedPoint(_T_41, 16) @[sqdist.scala 19:61]
    node _T_43 = mul(_T_39, _T_42) @[sqdist.scala 19:56]
    node _T_44 = sub(io.in2[2], io.in1[2]) @[sqdist.scala 19:51]
    node _T_45 = tail(_T_44, 1) @[sqdist.scala 19:51]
    node _T_46 = asFixedPoint(_T_45, 16) @[sqdist.scala 19:51]
    node _T_47 = sub(io.in2[2], io.in1[2]) @[sqdist.scala 19:61]
    node _T_48 = tail(_T_47, 1) @[sqdist.scala 19:61]
    node _T_49 = asFixedPoint(_T_48, 16) @[sqdist.scala 19:61]
    node _T_50 = mul(_T_46, _T_49) @[sqdist.scala 19:56]
    node _T_51 = sub(io.in2[3], io.in1[3]) @[sqdist.scala 19:51]
    node _T_52 = tail(_T_51, 1) @[sqdist.scala 19:51]
    node _T_53 = asFixedPoint(_T_52, 16) @[sqdist.scala 19:51]
    node _T_54 = sub(io.in2[3], io.in1[3]) @[sqdist.scala 19:61]
    node _T_55 = tail(_T_54, 1) @[sqdist.scala 19:61]
    node _T_56 = asFixedPoint(_T_55, 16) @[sqdist.scala 19:61]
    node _T_57 = mul(_T_53, _T_56) @[sqdist.scala 19:56]
    node _T_58 = sub(io.in2[4], io.in1[4]) @[sqdist.scala 19:51]
    node _T_59 = tail(_T_58, 1) @[sqdist.scala 19:51]
    node _T_60 = asFixedPoint(_T_59, 16) @[sqdist.scala 19:51]
    node _T_61 = sub(io.in2[4], io.in1[4]) @[sqdist.scala 19:61]
    node _T_62 = tail(_T_61, 1) @[sqdist.scala 19:61]
    node _T_63 = asFixedPoint(_T_62, 16) @[sqdist.scala 19:61]
    node _T_64 = mul(_T_60, _T_63) @[sqdist.scala 19:56]
    node _T_65 = sub(io.in2[5], io.in1[5]) @[sqdist.scala 19:51]
    node _T_66 = tail(_T_65, 1) @[sqdist.scala 19:51]
    node _T_67 = asFixedPoint(_T_66, 16) @[sqdist.scala 19:51]
    node _T_68 = sub(io.in2[5], io.in1[5]) @[sqdist.scala 19:61]
    node _T_69 = tail(_T_68, 1) @[sqdist.scala 19:61]
    node _T_70 = asFixedPoint(_T_69, 16) @[sqdist.scala 19:61]
    node _T_71 = mul(_T_67, _T_70) @[sqdist.scala 19:56]
    node _T_72 = sub(io.in2[6], io.in1[6]) @[sqdist.scala 19:51]
    node _T_73 = tail(_T_72, 1) @[sqdist.scala 19:51]
    node _T_74 = asFixedPoint(_T_73, 16) @[sqdist.scala 19:51]
    node _T_75 = sub(io.in2[6], io.in1[6]) @[sqdist.scala 19:61]
    node _T_76 = tail(_T_75, 1) @[sqdist.scala 19:61]
    node _T_77 = asFixedPoint(_T_76, 16) @[sqdist.scala 19:61]
    node _T_78 = mul(_T_74, _T_77) @[sqdist.scala 19:56]
    node _T_79 = sub(io.in2[7], io.in1[7]) @[sqdist.scala 19:51]
    node _T_80 = tail(_T_79, 1) @[sqdist.scala 19:51]
    node _T_81 = asFixedPoint(_T_80, 16) @[sqdist.scala 19:51]
    node _T_82 = sub(io.in2[7], io.in1[7]) @[sqdist.scala 19:61]
    node _T_83 = tail(_T_82, 1) @[sqdist.scala 19:61]
    node _T_84 = asFixedPoint(_T_83, 16) @[sqdist.scala 19:61]
    node _T_85 = mul(_T_81, _T_84) @[sqdist.scala 19:56]
    node _T_86 = sub(io.in2[8], io.in1[8]) @[sqdist.scala 19:51]
    node _T_87 = tail(_T_86, 1) @[sqdist.scala 19:51]
    node _T_88 = asFixedPoint(_T_87, 16) @[sqdist.scala 19:51]
    node _T_89 = sub(io.in2[8], io.in1[8]) @[sqdist.scala 19:61]
    node _T_90 = tail(_T_89, 1) @[sqdist.scala 19:61]
    node _T_91 = asFixedPoint(_T_90, 16) @[sqdist.scala 19:61]
    node _T_92 = mul(_T_88, _T_91) @[sqdist.scala 19:56]
    node _T_93 = sub(io.in2[9], io.in1[9]) @[sqdist.scala 19:51]
    node _T_94 = tail(_T_93, 1) @[sqdist.scala 19:51]
    node _T_95 = asFixedPoint(_T_94, 16) @[sqdist.scala 19:51]
    node _T_96 = sub(io.in2[9], io.in1[9]) @[sqdist.scala 19:61]
    node _T_97 = tail(_T_96, 1) @[sqdist.scala 19:61]
    node _T_98 = asFixedPoint(_T_97, 16) @[sqdist.scala 19:61]
    node _T_99 = mul(_T_95, _T_98) @[sqdist.scala 19:56]
    node _T_100 = add(_T_36, _T_43) @[sqdist.scala 20:22]
    node _T_101 = tail(_T_100, 1) @[sqdist.scala 20:22]
    node _T_102 = asFixedPoint(_T_101, 32) @[sqdist.scala 20:22]
    node _T_103 = add(_T_102, _T_50) @[sqdist.scala 20:22]
    node _T_104 = tail(_T_103, 1) @[sqdist.scala 20:22]
    node _T_105 = asFixedPoint(_T_104, 32) @[sqdist.scala 20:22]
    node _T_106 = add(_T_105, _T_57) @[sqdist.scala 20:22]
    node _T_107 = tail(_T_106, 1) @[sqdist.scala 20:22]
    node _T_108 = asFixedPoint(_T_107, 32) @[sqdist.scala 20:22]
    node _T_109 = add(_T_108, _T_64) @[sqdist.scala 20:22]
    node _T_110 = tail(_T_109, 1) @[sqdist.scala 20:22]
    node _T_111 = asFixedPoint(_T_110, 32) @[sqdist.scala 20:22]
    node _T_112 = add(_T_111, _T_71) @[sqdist.scala 20:22]
    node _T_113 = tail(_T_112, 1) @[sqdist.scala 20:22]
    node _T_114 = asFixedPoint(_T_113, 32) @[sqdist.scala 20:22]
    node _T_115 = add(_T_114, _T_78) @[sqdist.scala 20:22]
    node _T_116 = tail(_T_115, 1) @[sqdist.scala 20:22]
    node _T_117 = asFixedPoint(_T_116, 32) @[sqdist.scala 20:22]
    node _T_118 = add(_T_117, _T_85) @[sqdist.scala 20:22]
    node _T_119 = tail(_T_118, 1) @[sqdist.scala 20:22]
    node _T_120 = asFixedPoint(_T_119, 32) @[sqdist.scala 20:22]
    node _T_121 = add(_T_120, _T_92) @[sqdist.scala 20:22]
    node _T_122 = tail(_T_121, 1) @[sqdist.scala 20:22]
    node _T_123 = asFixedPoint(_T_122, 32) @[sqdist.scala 20:22]
    node _T_124 = add(_T_123, _T_99) @[sqdist.scala 20:22]
    node _T_125 = tail(_T_124, 1) @[sqdist.scala 20:22]
    node _T_126 = asFixedPoint(_T_125, 32) @[sqdist.scala 20:22]
    io.out <= _T_126 @[sqdist.scala 32:10]
    
