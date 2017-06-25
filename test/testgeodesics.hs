import Matrix
import Geodesics

--
eps = 1.0e-7

delta::Double->Double->Double
delta a b = abs(a-b)

-- test of se
testse :: [Double] -> Bool
testse (lat:(a:(s:xs))) =
  (all (<eps)) (zipWith delta [e] xs)
  where e = se (Matrix.toR lat) (Matrix.toR a) s

-- test of XY
testXY :: [Double] -> Bool
testXY (a:(s:(e:xs))) =
  (all (<eps) (zipWith delta [(theX (Matrix.toR a) s e),
                              (theY (Matrix.toR a) s e)] xs))

-- test of phiF
testphiF :: [Double] -> Bool
testphiF (x:(lat:xs)) =
  (all (<eps)) (zipWith delta [phiF x (Matrix.toR lat)] xs)

-- test of lambda2 and phi2
testlp2 :: [Double] -> Bool
testlp2 (y:(lon:(pf:xs))) =
  (all (<eps) (zipWith delta [(lambda2 y pf (Matrix.toR lon)),
                              (phi2 y pf)] xs))

-- test of reverseHeading
testRH::[Double]->Bool
testRH (y:(pf:(a:(es:xs)))) =
  (all (<eps) (zipWith delta [(reverseHeading y pf (Matrix.toR a) es)] xs))


--
--まとめ
--
testTgtPosition :: [Double] -> Bool
testTgtPosition (lat1:(lon1:(a:(s:xs)))) =
  (all (<eps) (zipWith delta
               (tgtPosition [(Matrix.toR lat1),(Matrix.toR lon1)] s
                (Matrix.toR a)) xs))
  
resultSetOfse = [
  [45,0,500000,0],
  [45,10,500000,0.000525471194246442],
  [45,20,500000,0.00098756280733788],
  [45,30,500000,0.0013305397709896],
  [45,40,500000,0.00151303400158432],
  [45,50,500000,0.00151303400158432],
  [45,60,500000,0.0013305397709896],
  [45,70,500000,0.00098756280733788],
  [45,80,500000,0.000525471194246442],
  [45,90,500000,0.000000000000000000188151671366704],
  [45,100,500000,-0.000525471194246442],
  [45,110,500000,-0.00098756280733788],
  [45,120,500000,-0.0013305397709896],
  [45,130,500000,-0.00151303400158432],
  [45,140,500000,-0.00151303400158432],
  [45,150,500000,-0.0013305397709896],
  [45,160,500000,-0.00098756280733788],
  [45,170,500000,-0.000525471194246441],
  [45,180,500000,-0.000000000000000000376303342733408],
  [45,190,500000,0.000525471194246442],
  [45,200,500000,0.00098756280733788],
  [45,210,500000,0.0013305397709896],
  [45,220,500000,0.00151303400158432],
  [45,230,500000,0.00151303400158432],
  [45,240,500000,0.0013305397709896],
  [45,250,500000,0.000987562807337879],
  [45,260,500000,0.000525471194246442],
  [45,270,500000,0.000000000000000000564455014100112],
  [45,280,500000,-0.000525471194246441],
  [45,290,500000,-0.000987562807337881],
  [45,300,500000,-0.0013305397709896],
  [45,310,500000,-0.00151303400158432],
  [45,320,500000,-0.00151303400158432],
  [45,330,500000,-0.0013305397709896],
  [45,340,500000,-0.000987562807337879],
  [45,350,500000,-0.000525471194246442],
  [0,45,500000,0.00154671193853207],
  [5,45,500000,0.00154655463779045],
  [10,45,500000,0.00154608756221699],
  [15,45,500000,0.00154532503942374],
  [20,45,500000,0.00154429044627193],
  [25,45,500000,0.00154301547349087],
  [30,45,500000,0.00154153913199761],
  [35,45,500000,0.00153990653482128],
  [40,45,500000,0.00153816749559295],
  [45,45,500000,0.0015363749898967],
  [50,45,500000,0.001534583529259],
  [55,45,500000,0.00153284749915687],
  [60,45,500000,0.00153121951222935],
  [65,45,500000,0.00152974882604344],
  [70,45,500000,0.00152847987151478],
  [75,45,500000,0.00152745093367033],
  [80,45,500000,0.00152669302112589],
  [85,45,500000,0.00152622895467861],
  [45,45,10000,0.000000614549995958679],
  [45,45,25000,0.00000384093747474175],
  [45,45,50000,0.000015363749898967],
  [45,45,75000,0.0000345684372726757],
  [45,45,100000,0.0000614549995958679],
  [45,45,125000,0.0000960234368685437],
  [45,45,150000,0.000138273749090703],
  [45,45,200000,0.000245819998383472],
  [45,45,250000,0.000384093747474175],
  [45,45,300000,0.000553094996362811],
  [45,45,350000,0.000752823745049382],
  [45,45,400000,0.000983279993533887],
  [45,45,450000,0.00124446374181633],
  [45,45,500000,0.0015363749898967],
  [45,135,10000,-0.000000614549995958679],
  [45,135,25000,-0.00000384093747474175],
  [45,135,50000,-0.000015363749898967],
  [45,135,75000,-0.0000345684372726757],
  [45,135,100000,-0.0000614549995958679],
  [45,135,125000,-0.0000960234368685437],
  [45,135,150000,-0.000138273749090703],
  [45,135,200000,-0.000245819998383472],
  [45,135,250000,-0.000384093747474175],
  [45,135,300000,-0.000553094996362811],
  [45,135,350000,-0.000752823745049382],
  [45,135,400000,-0.000983279993533887],
  [45,135,450000,-0.00124446374181633],
  [45,135,500000,-0.0015363749898967]]

resultSetOfXY = [
  [0,500000,0,500000,0],
  [10,500000,0.000525471194246442,492434.292211203,86737.8408157854],
  [20,500000,0.00098756280733788,469958.899183924,170855.404082398],
  [30,500000,0.0013305397709896,433234.458520718,249807.953126263],
  [40,500000,0.00151303400158432,383346.408062573,321200.629628409],
  [50,500000,0.00151303400158432,321780.155272991,382860.128307947],
  [60,500000,0.0013305397709896,250384.093747474,432901.82357797],
  [70,500000,0.00098756280733788,171319.406823707,469790.015997469],
  [80,500000,0.000525471194246442,86996.5848688247,492388.668653554],
  [90,500000,0.000000000000000000188151671366704,0.0000000000306788872024727,500000],
  [100,500000,-0.000525471194246442,-86996.5848688246,492388.668653554],
  [110,500000,-0.00098756280733788,-171319.406823707,469790.015997469],
  [120,500000,-0.0013305397709896,-250384.093747474,432901.82357797],
  [130,500000,-0.00151303400158432,-321780.155272991,382860.128307947],
  [140,500000,-0.00151303400158432,-383346.408062573,321200.629628409],
  [150,500000,-0.0013305397709896,-433234.458520718,249807.953126263],
  [160,500000,-0.00098756280733788,-469958.899183924,170855.404082398],
  [170,500000,-0.000525471194246441,-492434.292211203,86737.8408157854],
  [180,500000,-0.000000000000000000376303342733408,-500000,0.0000000000611696227335788],
  [190,500000,0.000525471194246442,-492434.292211203,-86737.8408157855],
  [200,500000,0.00098756280733788,-469958.899183924,-170855.404082398],
  [210,500000,0.0013305397709896,-433234.458520718,-249807.953126263],
  [220,500000,0.00151303400158432,-383346.408062573,-321200.629628409],
  [230,500000,0.00151303400158432,-321780.155272991,-382860.128307947],
  [240,500000,0.0013305397709896,-250384.093747474,-432901.82357797],
  [250,500000,0.000987562807337879,-171319.406823707,-469790.015997469],
  [260,500000,0.000525471194246442,-86996.5848688247,-492388.668653554],
  [270,500000,0.000000000000000000564455014100112,-0.0000000000920366616074182,-500000],
  [280,500000,-0.000525471194246441,86996.5848688245,-492388.668653555],
  [290,500000,-0.000987562807337881,171319.406823707,-469790.015997469],
  [300,500000,-0.0013305397709896,250384.093747474,-432901.82357797],
  [310,500000,-0.00151303400158432,321780.155272991,-382860.128307947],
  [320,500000,-0.00151303400158432,383346.408062573,-321200.629628409],
  [330,500000,-0.0013305397709896,433234.458520717,-249807.953126263],
  [340,500000,-0.000987562807337879,469958.899183924,-170855.404082398],
  [350,500000,-0.000525471194246442,492434.292211203,-86737.8408157855],
  [360,500000,-0.000000000000000000752606685466816,500000,-0.000000000122339245467158],
  [45,500000,0.00154671193853207,353917.954093367,353371.108843227],
  [45,500000,0.00154655463779045,353917.917017226,353371.127381298],
  [45,500000,0.00154608756221699,353917.806926458,353371.182426682],
  [45,500000,0.00154532503942374,353917.627198112,353371.272290855],
  [45,500000,0.00154429044627193,353917.383342167,353371.394218827],
  [45,500000,0.00154301547349087,353917.082828201,353371.54447581],
  [45,500000,0.00154153913199761,353916.734851174,353371.718464324],
  [45,500000,0.00153990653482128,353916.350044329,353371.910867746],
  [45,500000,0.00153816749559295,353915.940148852,353372.115815485],
  [45,500000,0.0015363749898967,353915.517651208,353372.327064307],
  [45,500000,0.001534583529259,353915.095399886,353372.538189968],
  [45,500000,0.00153284749915687,353914.686213667,353372.742783077],
  [45,500000,0.00153121951222935,353914.302493468,353372.934643177],
  [45,500000,0.00152974882604344,353913.95584941,353373.107965206],
  [45,500000,0.00152847987151478,353913.656753959,353373.257512931],
  [45,500000,0.00152745093367033,353913.414230983,353373.378774419],
  [45,500000,0.00152669302112589,353913.235589283,353373.468095269],
  [45,25000,0.00000384093747474175,17677.7147955459,17677.6468967226],
  [45,50000,0.000015363749898967,35355.7011863853,35355.1579957984],
  [45,100000,0.0000614549995958679,70713.5751351182,70709.229610423],
  [45,250000,0.000384093747474175,176821.961178879,176754.062355516],
  [45,500000,0.0015363749898967,353915.517651208,353372.327064307],
  [135,25000,-0.00000384093747474175,-17677.7147955459,17677.6468967226],
  [135,50000,-0.000015363749898967,-35355.7011863853,35355.1579957984],
  [135,100000,-0.0000614549995958679,-70713.5751351182,70709.229610423],
  [135,250000,-0.000384093747474175,-176821.961178879,176754.062355516],
  [135,500000,-0.0015363749898967,-353915.517651208,353372.327064307],
  [45,25000,0.00000384093747474175,17677.7147955459,17677.6468967226],
  [45,50000,0.000015363749898967,35355.7011863853,35355.1579957984],
  [45,100000,0.0000614549995958679,70713.5751351182,70709.229610423],
  [45,250000,0.000384093747474175,176821.961178879,176754.062355516],
  [45,500000,0.0015363749898967,353915.517651208,353372.327064307],
  [135,25000,-0.00000384093747474175,-17677.7147955459,17677.6468967226],
  [135,50000,-0.000015363749898967,-35355.7011863853,35355.1579957984],
  [135,100000,-0.0000614549995958679,-70713.5751351182,70709.229610423],
  [135,250000,-0.000384093747474175,-176821.961178879,176754.062355516],
  [135,500000,-0.0015363749898967,-353915.517651208,353372.327064307]]

resultSetOfPhiF = [
  [500000,45,0.863892374541938],
  [492434.292211203,45,0.862705106137765],
  [469958.899183924,45,0.859178013619651],
  [433234.458520718,45,0.853414532344306],
  [383346.408062573,45,0.84558464090454],
  [321780.155272991,45,0.835921017959387],
  [250384.093747474,45,0.824713305248294],
  [171319.406823707,45,0.812300302518728],
  [86996.5848688247,45,0.799060073410705],
  [0.0000000000306788872024727,45,0.785398163397448],
  [-86996.5848688246,45,0.771734372710256],
  [-171319.406823707,45,0.758488732304773],
  [-250384.093747474,45,0.746067450170041],
  [-321780.155272991,45,0.734849599723927],
  [-383346.408062573,45,0.725175210728578],
  [-433234.458520718,45,0.717335223432258],
  [-469958.899183924,45,0.711563526913356],
  [-492434.292211203,45,0.708031079520629],
  [-500000,45,0.706841952725663],
  [-492434.292211203,45,0.708031079520629],
  [-469958.899183924,45,0.711563526913356],
  [-433234.458520718,45,0.717335223432258],
  [-383346.408062573,45,0.725175210728578],
  [-321780.155272991,45,0.734849599723927],
  [-250384.093747474,45,0.746067450170041],
  [-171319.406823707,45,0.758488732304773],
  [-86996.5848688247,45,0.771734372710256],
  [-0.0000000000920366616074182,45,0.785398163397448],
  [86996.5848688245,45,0.799060073410705],
  [171319.406823707,45,0.812300302518728],
  [250384.093747474,45,0.824713305248294],
  [321780.155272991,45,0.835921017959387],
  [383346.408062573,45,0.84558464090454],
  [433234.458520717,45,0.853414532344306],
  [469958.899183924,45,0.859178013619651],
  [492434.292211203,45,0.862705106137765],
  [500000,45,0.863892374541938],
  [353917.954093367,0,0.0558626266775874],
  [353917.917017226,5,0.143122113568443],
  [353917.806926458,10,0.230373298669615],
  [353917.627198112,15,0.31761664942417],
  [353917.383342167,20,0.404852871918977],
  [353917.082828201,25,0.492082888967226],
  [353916.734851174,30,0.579307811589639],
  [353916.350044329,35,0.666528904818506],
  [353915.940148852,40,0.753747548916172],
  [353915.517651208,45,0.840965197227564],
  [353915.095399886,50,0.928183331972452],
  [353914.686213667,55,1.01540341932647],
  [353914.302493468,60,1.10262686514167],
  [353913.95584941,65,1.1898549726198],
  [353913.656753959,70,1.27708890317751],
  [353913.414230983,75,1.36432964163733],
  [353913.235589283,80,1.45157796674403],
  [17677.7147955459,45,0.788174417133758],
  [35355.7011863853,45,0.79095063586487],
  [70713.5751351182,45,0.796503138911362],
  [176821.961178879,45,0.81316424309227],
  [353915.517651208,45,0.840965197227564],
  [-17677.7147955459,45,0.782621832002982],
  [-35355.7011863853,45,0.779845380295006],
  [-70713.5751351182,45,0.774291945305148],
  [-176821.961178879,45,0.757624315913223],
  [-353915.517651208,45,0.729800034558669],
  [17677.7147955459,135,2.35897082158681],
  [35355.7011863853,135,2.36174727329479],
  [70713.5751351182,135,2.36730070828464],
  [176821.961178879,135,2.38396833767657],
  [353915.517651208,135,2.41179261903112],
  [-17677.7147955459,135,2.35341823645603],
  [-35355.7011863853,135,2.35064201772492],
  [-70713.5751351182,135,2.34508951467843],
  [-176821.961178879,135,2.32842841049752],
  [-353915.517651208,135,2.30062745636223]]

resultSetOflp = [
  [0,0,0.863892374541938,0,0.863892374541938],
  [86737.8408157854,0,0.862705106137765,0.0208673576336707,0.862597226756333],
  [170855.404082398,0,0.859178013619651,0.0409267532374123,0.858762481659812],
  [249807.953126263,0,0.853414532344306,0.0594232190605882,0.852536736050785],
  [321200.629628409,0,0.84558464090454,0.0757000573824722,0.844156550748624],
  [382860.128307947,0,0.835921017959387,0.0892307844626297,0.83393159091624],
  [432901.82357797,0,0.824713305248294,0.0996354597813323,0.82222687859593],
  [469790.015997469,0,0.812300302518728,0.106682421281472,0.809444214761214],
  [492388.668653554,0,0.799060073410705,0.110278737943113,0.796004443621692],
  [500000,0,0.785398163397448,0.110453603505344,0.782331644526724],
  [492388.668653554,0,0.771734372710256,0.107338635884381,0.768839746425408],
  [469790.015997469,0,0.758488732304773,0.101148105903516,0.75592159832032],
  [432901.82357797,0,0.746067450170041,0.0921609829065457,0.743940245892293],
  [382860.128307947,0,0.734849599723927,0.0807056813025389,0.733222046341737],
  [321200.629628409,0,0.725175210728578,0.0671476667174955,0.72405125468449],
  [249807.953126263,0,0.717335223432258,0.0518796455611949,0.716665781630584],
  [170855.404082398,0,0.711563526913356,0.0353138601799517,0.711253910233119],
  [86737.8408157854,0,0.708031079520629,0.0178759654001578,0.707951836191232],
  [0.0000000000611696227335788,0,0.706841952725663,0.000000000000000012594368238888,0.706841952725663],
  [-86737.8408157855,0,0.708031079520629,-0.0178759654001578,0.707951836191232],
  [-170855.404082398,0,0.711563526913356,-0.0353138601799516,0.711253910233119],
  [-249807.953126263,0,0.717335223432258,-0.0518796455611949,0.716665781630584],
  [-321200.629628409,0,0.725175210728578,-0.0671476667174955,0.72405125468449],
  [-382860.128307947,0,0.734849599723927,-0.0807056813025388,0.733222046341737],
  [-432901.82357797,0,0.746067450170041,-0.0921609829065456,0.743940245892293],
  [-469790.015997469,0,0.758488732304773,-0.101148105903516,0.75592159832032],
  [-492388.668653554,0,0.771734372710256,-0.107338635884381,0.768839746425408],
  [-500000,0,0.785398163397448,-0.110453603505344,0.782331644526724],
  [-492388.668653555,0,0.799060073410705,-0.110278737943113,0.796004443621692],
  [-469790.015997469,0,0.812300302518728,-0.106682421281472,0.809444214761214],
  [-432901.82357797,0,0.824713305248294,-0.0996354597813323,0.82222687859593],
  [-382860.128307947,0,0.835921017959387,-0.0892307844626297,0.83393159091624],
  [-321200.629628409,0,0.84558464090454,-0.0757000573824723,0.844156550748624],
  [-249807.953126263,0,0.853414532344306,-0.0594232190605883,0.852536736050785],
  [-170855.404082398,0,0.859178013619651,-0.0409267532374123,0.858762481659812],
  [-86737.8408157855,0,0.862705106137765,-0.0208673576336707,0.862597226756333],
  [-0.000000000122339245467158,0,0.863892374541938,-0.0000000000000000294755725055588,0.863892374541938],
  [0,30,0.863892374541938,0.523598775598299,0.863892374541938],
  [86737.8408157854,30,0.862705106137765,0.544466133231969,0.862597226756333],
  [170855.404082398,30,0.859178013619651,0.564525528835711,0.858762481659812],
  [249807.953126263,30,0.853414532344306,0.583021994658887,0.852536736050785],
  [321200.629628409,30,0.84558464090454,0.599298832980771,0.844156550748624],
  [382860.128307947,30,0.835921017959387,0.612829560060928,0.83393159091624],
  [432901.82357797,30,0.824713305248294,0.623234235379631,0.82222687859593],
  [469790.015997469,30,0.812300302518728,0.630281196879771,0.809444214761214],
  [492388.668653554,30,0.799060073410705,0.633877513541411,0.796004443621692],
  [500000,30,0.785398163397448,0.634052379103642,0.782331644526724],
  [492388.668653554,30,0.771734372710256,0.63093741148268,0.768839746425408],
  [469790.015997469,30,0.758488732304773,0.624746881501814,0.75592159832032],
  [432901.82357797,30,0.746067450170041,0.615759758504845,0.743940245892293],
  [382860.128307947,30,0.734849599723927,0.604304456900838,0.733222046341737],
  [321200.629628409,30,0.725175210728578,0.590746442315794,0.72405125468449],
  [249807.953126263,30,0.717335223432258,0.575478421159494,0.716665781630584],
  [170855.404082398,30,0.711563526913356,0.55891263577825,0.711253910233119],
  [86737.8408157854,30,0.708031079520629,0.541474740998457,0.707951836191232],
  [0.0000000000611696227335788,30,0.706841952725663,0.523598775598299,0.706841952725663],
  [-86737.8408157855,30,0.708031079520629,0.505722810198141,0.707951836191232],
  [-170855.404082398,30,0.711563526913356,0.488284915418347,0.711253910233119],
  [-249807.953126263,30,0.717335223432258,0.471719130037104,0.716665781630584],
  [-321200.629628409,30,0.725175210728578,0.456451108880803,0.72405125468449],
  [-382860.128307947,30,0.734849599723927,0.44289309429576,0.733222046341737],
  [-432901.82357797,30,0.746067450170041,0.431437792691753,0.743940245892293],
  [-469790.015997469,30,0.758488732304773,0.422450669694783,0.75592159832032],
  [-492388.668653554,30,0.771734372710256,0.416260139713918,0.768839746425408],
  [-500000,30,2.35619449019234,0.634052379103642,2.35926100906307],
  [-492388.668653555,30,2.36985828087954,0.63093741148268,2.37275290716438],
  [-469790.015997469,30,2.38310392128502,0.624746881501814,2.38567105526947],
  [-432901.82357797,30,2.39552520341975,0.615759758504845,2.3976524076975],
  [-382860.128307947,30,2.40674305386587,0.604304456900838,2.40837060724806],
  [-321200.629628409,30,2.41641744286122,0.590746442315794,2.4175413989053],
  [-249807.953126263,30,2.42425743015754,0.575478421159494,2.42492687195921],
  [-170855.404082398,30,2.43002912667644,0.55891263577825,2.43033874335667],
  [-86737.8408157855,30,2.43356157406916,0.541474740998457,2.43364081739856],
  [-0.000000000122339245467158,30,2.43475070086413,0.523598775598299,2.43475070086413]]
  
resultSetOfRH = [
  [0,0.863892374541938,0,0,3.14159265358979],
  [86737.8408157854,0.862705106137765,10,0.000525471194246442,3.3314505625009],
  [170855.404082398,0.859178013619651,20,0.00098756280733788,3.52066131537959],
  [249807.953126263,0.853414532344306,30,0.0013305397709896,3.7086266104867],
  [321200.629628409,0.84558464090454,40,0.00151303400158432,3.89483823904518],
  [382860.128307947,0.835921017959387,50,0.00151303400158432,4.07890616974865],
  [432901.82357797,0.824713305248294,60,0.0013305397709896,4.26057123034704],
  [469790.015997469,0.812300302518728,70,0.00098756280733788,4.43970341226534],
  [492388.668653554,0.799060073410705,80,0.000525471194246442,4.61628911852726],
  [500000,0.785398163397448,90,0.000000000000000000188151671366704,4.79041163115515],
  [492388.668653554,0.771734372710256,100,-0.000525471194246442,4.9622288866518],
  [469790.015997469,0.758488732304773,110,-0.00098756280733788,5.13195178068585],
  [432901.82357797,0.746067450170041,120,-0.0013305397709896,5.29982513106972],
  [382860.128307947,0.734849599723927,130,-0.00151303400158432,5.46611241625168],
  [321200.629628409,0.725175210728578,140,-0.00151303400158432,5.63108461546121],
  [249807.953126263,0.717335223432258,150,-0.0013305397709896,5.79501293361357],
  [170855.404082398,0.711563526913356,160,-0.00098756280733788,5.95816487016852],
  [86737.8408157854,0.708031079520629,170,-0.000525471194246441,6.1208029382773],
  [0.0000000000611696227335788,0.706841952725663,180,-0.000000000000000000376303342733408,6.28318530717959],
  [-86737.8408157855,0.708031079520629,190,0.000525471194246442,6.44556767608187],
  [-170855.404082398,0.711563526913356,200,0.00098756280733788,6.60820574419065],
  [-249807.953126263,0.717335223432258,210,0.0013305397709896,6.7713576807456],
  [-321200.629628409,0.725175210728578,220,0.00151303400158432,6.93528599889797],
  [-382860.128307947,0.734849599723927,230,0.00151303400158432,7.10025819810749],
  [-432901.82357797,0.746067450170041,240,0.0013305397709896,7.26654548328945],
  [-469790.015997469,0.758488732304773,250,0.000987562807337879,7.43441883367333],
  [-492388.668653554,0.771734372710256,260,0.000525471194246442,7.60414172770737],
  [-500000,0.785398163397448,270,0.000000000000000000564455014100112,7.77595898320403],
  [-492388.668653555,0.799060073410705,280,-0.000525471194246441,7.95008149583191],
  [-469790.015997469,0.812300302518728,290,-0.000987562807337881,8.12666720209384],
  [-432901.82357797,0.824713305248294,300,-0.0013305397709896,8.30579938401214],
  [-382860.128307947,0.835921017959387,310,-0.00151303400158432,8.48746444461052],
  [-321200.629628409,0.84558464090454,320,-0.00151303400158432,8.671532375314],
  [-249807.953126263,0.853414532344306,330,-0.0013305397709896,8.85774400387247],
  [-170855.404082398,0.859178013619651,340,-0.000987562807337879,9.04570929897958],
  [-86737.8408157855,0.862705106137765,350,-0.000525471194246442,9.23492005185827],
  [-0.000000000122339245467158,0.863892374541938,360,-0.000000000000000000752606685466816,9.42477796076938],
  [0,0.863892374541938,0,0,3.14159265358979],
  [86737.8408157854,0.862705106137765,10,0.000525471194246442,3.3314505625009],
  [170855.404082398,0.859178013619651,20,0.00098756280733788,3.52066131537959],
  [249807.953126263,0.853414532344306,30,0.0013305397709896,3.7086266104867],
  [321200.629628409,0.84558464090454,40,0.00151303400158432,3.89483823904518],
  [382860.128307947,0.835921017959387,50,0.00151303400158432,4.07890616974865],
  [432901.82357797,0.824713305248294,60,0.0013305397709896,4.26057123034704],
  [469790.015997469,0.812300302518728,70,0.00098756280733788,4.43970341226534],
  [492388.668653554,0.799060073410705,80,0.000525471194246442,4.61628911852726],
  [500000,0.785398163397448,90,0.000000000000000000188151671366704,4.79041163115515],
  [492388.668653554,0.771734372710256,100,-0.000525471194246442,4.9622288866518],
  [469790.015997469,0.758488732304773,110,-0.00098756280733788,5.13195178068585],
  [432901.82357797,0.746067450170041,120,-0.0013305397709896,5.29982513106972],
  [382860.128307947,0.734849599723927,130,-0.00151303400158432,5.46611241625168],
  [321200.629628409,0.725175210728578,140,-0.00151303400158432,5.63108461546121],
  [249807.953126263,0.717335223432258,150,-0.0013305397709896,5.79501293361357],
  [170855.404082398,0.711563526913356,160,-0.00098756280733788,5.95816487016852],
  [86737.8408157854,0.708031079520629,170,-0.000525471194246441,6.1208029382773],
  [0.0000000000611696227335788,0.706841952725663,180,-0.000000000000000000376303342733408,6.28318530717959],
  [-86737.8408157855,0.708031079520629,190,0.000525471194246442,6.44556767608187],
  [-170855.404082398,0.711563526913356,200,0.00098756280733788,6.60820574419065],
  [-249807.953126263,0.717335223432258,210,0.0013305397709896,6.7713576807456],
  [-321200.629628409,0.725175210728578,220,0.00151303400158432,6.93528599889797],
  [-382860.128307947,0.734849599723927,230,0.00151303400158432,7.10025819810749],
  [-432901.82357797,0.746067450170041,240,0.0013305397709896,7.26654548328945],
  [-469790.015997469,0.758488732304773,250,0.000987562807337879,7.43441883367333],
  [-492388.668653554,0.771734372710256,260,0.000525471194246442,7.60414172770737],
  [-500000,2.35619449019234,270,0.000000000000000000564455014100112,7.93200428474494],
  [-492388.668653555,2.36985828087954,280,-0.000525471194246441,8.1038215402416],
  [-469790.015997469,2.38310392128502,290,-0.000987562807337881,8.27354443427564],
  [-432901.82357797,2.39552520341975,300,-0.0013305397709896,8.44141778465952],
  [-382860.128307947,2.40674305386587,310,-0.00151303400158432,8.60770506984148],
  [-321200.629628409,2.41641744286122,320,-0.00151303400158432,8.772677269051],
  [-249807.953126263,2.42425743015754,330,-0.0013305397709896,8.93660558720336],
  [-170855.404082398,2.43002912667644,340,-0.000987562807337879,9.09975752375831],
  [-86737.8408157855,2.43356157406916,350,-0.000525471194246442,9.2623955918671],
  [-0.000000000122339245467158,2.43475070086413,360,-0.000000000000000000752606685466816,9.42477796076938]]
  
resultSetOfTgt = [
  [45,0,0,500000,0.863892374541938,0],
  [45,0,10,500000,0.862597226756333,0.0208673576336707],
  [45,0,20,500000,0.858762481659812,0.0409267532374123],
  [45,0,30,500000,0.852536736050785,0.0594232190605882],
  [45,0,40,500000,0.844156550748624,0.0757000573824722],
  [45,0,50,500000,0.83393159091624,0.0892307844626297],
  [45,0,60,500000,0.82222687859593,0.0996354597813323],
  [45,0,70,500000,0.809444214761214,0.106682421281472],
  [45,0,80,500000,0.796004443621692,0.110278737943113],
  [45,0,90,500000,0.782331644526724,0.110453603505344],
  [45,0,100,500000,0.768839746425408,0.107338635884381],
  [45,0,110,500000,0.75592159832032,0.101148105903516],
  [45,0,120,500000,0.743940245892293,0.0921609829065457],
  [45,0,130,500000,0.733222046341737,0.0807056813025389],
  [45,0,140,500000,0.72405125468449,0.0671476667174955],
  [45,0,150,500000,0.716665781630584,0.0518796455611949],
  [45,0,160,500000,0.711253910233119,0.0353138601799517],
  [45,0,170,500000,0.707951836191232,0.0178759654001578],
  [45,0,180,500000,0.706841952725663,0.000000000000000012594368238888],
  [45,0,190,500000,0.707951836191232,-0.0178759654001578],
  [45,0,200,500000,0.711253910233119,-0.0353138601799516],
  [45,0,210,500000,0.716665781630584,-0.0518796455611949],
  [45,0,220,500000,0.72405125468449,-0.0671476667174955],
  [45,0,230,500000,0.733222046341737,-0.0807056813025388],
  [45,0,240,500000,0.743940245892293,-0.0921609829065456],
  [45,0,250,500000,0.75592159832032,-0.101148105903516],
  [45,0,260,500000,0.768839746425408,-0.107338635884381],
  [45,0,270,500000,0.782331644526724,-0.110453603505344],
  [45,0,280,500000,0.796004443621692,-0.110278737943113],
  [45,0,290,500000,0.809444214761214,-0.106682421281472],
  [45,0,300,500000,0.82222687859593,-0.0996354597813323],
  [45,0,310,500000,0.83393159091624,-0.0892307844626297],
  [45,0,320,500000,0.844156550748624,-0.0757000573824723],
  [45,0,330,500000,0.852536736050785,-0.0594232190605883],
  [45,0,340,500000,0.858762481659812,-0.0409267532374123],
  [45,0,350,500000,0.862597226756333,-0.0208673576336707],
  [45,0,360,500000,0.863892374541938,-0.0000000000000000294755725055588],
  [45,30,0,500000,0.863892374541938,0.523598775598299],
  [45,30,10,500000,0.862597226756333,0.544466133231969],
  [45,30,20,500000,0.858762481659812,0.564525528835711],
  [45,30,30,500000,0.852536736050785,0.583021994658887],
  [45,30,40,500000,0.844156550748624,0.599298832980771],
  [45,30,50,500000,0.83393159091624,0.612829560060928],
  [45,30,60,500000,0.82222687859593,0.623234235379631],
  [45,30,70,500000,0.809444214761214,0.630281196879771],
  [45,30,80,500000,0.796004443621692,0.633877513541411],
  [45,30,90,500000,0.782331644526724,0.634052379103642],
  [45,30,100,500000,0.768839746425408,0.63093741148268],
  [45,30,110,500000,0.75592159832032,0.624746881501814],
  [45,30,120,500000,0.743940245892293,0.615759758504845],
  [45,30,130,500000,0.733222046341737,0.604304456900838],
  [45,30,140,500000,0.72405125468449,0.590746442315794],
  [45,30,150,500000,0.716665781630584,0.575478421159494],
  [45,30,160,500000,0.711253910233119,0.55891263577825],
  [45,30,170,500000,0.707951836191232,0.541474740998457],
  [45,30,180,500000,0.706841952725663,0.523598775598299],
  [45,30,190,500000,0.707951836191232,0.505722810198141],
  [45,30,200,500000,0.711253910233119,0.488284915418347],
  [45,30,210,500000,0.716665781630584,0.471719130037104],
  [45,30,220,500000,0.72405125468449,0.456451108880803],
  [45,30,230,500000,0.733222046341737,0.44289309429576],
  [45,30,240,500000,0.743940245892293,0.431437792691753],
  [45,30,250,500000,0.75592159832032,0.422450669694783],
  [45,30,260,500000,0.768839746425408,0.416260139713918],
  [135,30,270,500000,2.35926100906307,0.634052379103642],
  [135,30,280,500000,2.37275290716438,0.63093741148268],
  [135,30,290,500000,2.38567105526947,0.624746881501814],
  [135,30,300,500000,2.3976524076975,0.615759758504845],
  [135,30,310,500000,2.40837060724806,0.604304456900838],
  [135,30,320,500000,2.4175413989053,0.590746442315794],
  [135,30,330,500000,2.42492687195921,0.575478421159494],
  [135,30,340,500000,2.43033874335667,0.55891263577825],
  [135,30,350,500000,2.43364081739856,0.541474740998457],
  [135,30,360,500000,2.43475070086413,0.523598775598299]]
  