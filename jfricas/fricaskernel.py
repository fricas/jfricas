# -*- coding: utf-8 -*-

# Reference:
# http://jupyter-client.readthedocs.io/en/stable/wrapperkernels.html

# Revision history
# 17-JUL-2019 ........ python3: replace <> by !=
# 21-JUL-2019 ........ banner = "FriCAS Jupyter Kernel" /removed Axiom, SPAD
#             ........ reformatting spadcmd using par 85j.
# 23-JUL-2019 ........ Added global pid (process id of fricas+HT)
#             ........ do_shutdown: self.server.put(")quit")
#                      pid.terminate() # terminate fricas+HT
# 30-JUL-2019 ........ restart=False in do_shutdown
#             ........ Language: spad (removed SPAD).
# 08-AUG-2019 ........ Added terminal feature: ['term','-?']+['fricas',...]
#                      Added: user config section
#                      Added: subprocess.run (! prefix for shell commands)
#                      Removed: import imp --deprecated since 3.4
#                      Tidy up.
# 13-AUG-2019 ........ V 0.2.10
#

from ipykernel.kernelbase import Kernel
from subprocess import Popen, run, PIPE, STDOUT
import requests
import json
import os

path = os.path.abspath(__file__)
dir_path = os.path.dirname(path)


__version__ = '0.2.10'

# ********************************
# BEGIN user configuration options
# ********************************
pycmd = ')python'
shcmd = '!'
shutd = ')shutdown'

fricas_start_options = '-noht'   ### -nox blocks if draw is used (others?)
fricas_terminal = []             ###  E.g. ['xterm','-e'] for 'xterm'

shell_timeout = 15 # Timeout for shell commands in secs.

html_prefix = '$HTML$'

# LaTeX color/size parameters
type_color = r"blue"
type_size = r"\scriptsize"
tex_color = r"black"
tex_size = r"\normalsize"

# Templates (TeX)
pretex1 = r"\(\def\sp{^}\def\sb{_}\def\leqno(#1){}\)"
pretex2 = r"\(\def\erf\{\mathrm{erf}}\def\sinh{\mathrm{sinh}}\)"
pretex3 = r"\(\def\zag#1#2{{{ \left.{#1}\right|}\over{\left|{#2}\right.}}}\)"
pretex4 = r"\(\require{color}\)"
pretex = pretex1+pretex2+pretex3+pretex4
ljax = r"$$"  # variants: r"\("
rjax = r"$$"  #           r"\)"

# texout_types.format(tex_color,tex_size,tex,type_color,type_size,type)
texout_types = r"""
{{\color{{{0}}} {1} {2}}} \\[0.9ex] {{\color{{{3}}} {4} \text{{{5}}}}} \\
"""

# texout.format(tex_color,tex_size,tex)
texout = r"""
{{\color{{{0}}} {1} {2}}}
"""
# ***************
# END user config
# ***************

def get_free_port():
        import socket
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(("",0))
        s.listen(1)
        port = s.getsockname()[1]
        s.close()
        return port

# Get new free port
htport=str(get_free_port())

# PID of FriCAS+HT
pid=None 


class httpSPAD():

    def __init__(self, url = 'http://localhost:{0}/json'.format(htport)):
        # Store parameters
        self.url = url
        self.output = None
        
        
    def put(self, code):
        # POST request
        payload = {'code': code}
        r = requests.post(self.url, data=payload)
        data = r.text
        data = data.replace('\r','\\r')
        data = data.replace('\n','\\n')
        self.output = json.loads(data.rstrip('\\n'))
        return(r)
        

class SPAD(Kernel):
    implementation = 'spad'
    implementation_version = __version__
    language = 'spad'
    language_version = '0.1'
    language_info = {'name': 'spad', 'mimetype': 'text/plain',
                     'file_extension': '.input',}
    banner = "FriCAS Jupyter Kernel"
    
    
    def __init__(self, **kwargs):
        Kernel.__init__(self, **kwargs)
        self.server = httpSPAD()
        self.spadcmds = spad_commands


    def do_execute(self, code, silent, store_history=True, 
                   user_expressions=None, allow_stdin=False):

        # pre-process input
        if code.startswith(pycmd):
            # Python code in cell
            self.output = str(eval(code[len(pycmd):].lstrip()))
            pyeval = {'name': 'stdout', 'text': self.output}
            self.send_response(self.iopub_socket, 'stream', pyeval)
            return

        if code.startswith(shutd):
            # Shutdown requested
            self.do_shutdown(False)
            
        if code.startswith(shcmd):
            # Shell code in cell
            cmd = str(code[len(shcmd):].lstrip())
            cp = run(cmd, stdout=PIPE, stderr=STDOUT, timeout=shell_timeout, shell=True)
            self.output = cp.stdout.decode()
            sheval = {'name': 'stdout', 'text': self.output}
            self.send_response(self.iopub_socket, 'stream', sheval)
            return

        # send code to hunchentoot and get response
        r = self.server.put(code)
        data = dict()
        if r.ok:

            ff = self.server.output['format-flags']

            if ff['tex']=='true':
                tex = self.server.output['tex']
                typ = self.server.output['spad-type']
                data['text/latex'] = makeTeXType(tex,typ)

            if ff['html']=='true':
                data['text/html'] = self.server.output['html']

            if ff['mathml']=='true':
                data['text/mathml'] = self.server.output['mathml']
                        
            
        charybdis = self.server.output['charybdis']
        standard_output = self.server.output['stdout']
        spadtype = self.server.output['spad-type'] 


        if not silent:
            if ff['algebra'] == 'true':
                if charybdis != "":
                    alg = self.server.output['algebra'].strip().strip('"')
                    if alg.startswith(html_prefix):
                        data['text/html']  = alg[len(html_prefix):].rstrip().rstrip('"')
                        stdout = {'name': 'stdout', 'text': standard_output}
                    else:
                        stdout = {'name': 'stdout', 'text': charybdis}
                else:
                    stdout = {'name': 'stdout', 'text': standard_output}
                self.send_response(self.iopub_socket, 'stream', stdout)
            else:
                if standard_output != "":
                    stdout = {'name': 'stdout', 'text': standard_output}
                    self.send_response(self.iopub_socket, 'stream', stdout)
                
            # Error handling (red)    
            if charybdis.startswith("error"):
                stderr = {'name': 'stderr', 'text': standard_output}
                self.send_response(self.iopub_socket, 'stream', stderr)
            

            # Display LaTeX, HTML, MathML ...
            display_data = {'data':data, 'metadata':{}}
            self.send_response(self.iopub_socket, 'display_data', display_data)


        return {'status': 'ok',
                # The base class increments the execution count
                'execution_count': self.execution_count,
                'payload': [],
                'user_expressions': {},
               }

    def do_complete(self, code, cursor_pos):
        code = code[:cursor_pos]
        default = {'matches': [], 'cursor_start': 0,
                   'cursor_end': cursor_pos, 'metadata': dict(),
                   'status': 'ok'}

        if not code or code[-1] == ' ':
            return default

        tokens = code.replace(';', ' ').split()
        if not tokens:
            return default

        token = tokens[-1]
        start = cursor_pos - len(token)  
        
        matches = [m for m in self.spadcmds if m.startswith(token)]

        return {'matches': matches, 'cursor_start': start,
                'cursor_end': cursor_pos, 'metadata': dict(),'status': 'ok'}
                

    def do_shutdown(self, restart):
        "Changes in 5.0: <data> replaced by <text>"
        output = "-- Bye. Kernel shutdown "
        stream_content = {'name': 'stdout', 'text': output}
        self.send_response(self.iopub_socket, 'stream', stream_content)
        try:
            r = self.server.put(")quit")
            pid.terminate() # terminate fricas+HT
        except:
            print("Go down anyway ...")
        return {'restart': False}
        



# Kernel spec kernel.json file for this:
#
# {"argv": ["python3", "-m","jfricas.fricaskernel","-f", "{connection_file}"],
#   "display_name": "FriCAS", "language": "spad"}
#    
# install it using e.g
#   jupyter kernelspec install </path/to/dir-containing-kernel-json>. 
# Place your kernel module anywhere Python can import it 
# (try current directory for testing). 
# Finally, you can run your kernel using 
#  jupyter console --kernel=jfricas. 
# 

# ===================
# SPAD -> JSON Output 
# ===================
#
#  { "input":"D(x^n,x,4)",
#    "multiline?":"false",
#    "spad-type":"",
#    "algebra":"",
#    "charybdis":"",
#    "tex":"",
#    "html":"",
#    "mathml":"",
#    "formula":"",
#    "fortran":"",
#    "texmacs":"",
#    "openmath":"",
#    "format-flags": 
#        {"algebra":"true",
#         "tex":"false",
#         "html":"false",
#         "mathml":"false",
#         "formula":"false",
#         "fortran":"false",
#         "texmcas":"false",
#         "openmath":"false"}}


def makeTeX(rawtex):
    r = rawtex.strip().strip('$$')
    r = texout.format(tex_color,tex_size,r)
    r = pretex + ljax + r + rjax
    return r

def makeTeXType(rawtex,rawtype):
    r = rawtex.strip().strip('$$')
    r = texout_types.format(tex_color,tex_size,r,type_color,type_size,rawtype)
    r = pretex + ljax + r + rjax
    return r    
    

### cat spadcmd.txt | par 85j > spadcmd85.txt

command_list="""
AND  Aleph  An   And  B1solve  BY  BasicMethod  Beta  BumInSepFFE   Chi  Ci  D  Delta
DiffAction  DiffC  EQ  EXPRR2F  Ei  F  F2EXPRR  F2FG  FG2F  FormatArabic  FormatRoman
Frobenius GE GF2FG  GT Gamma GospersMethod HP_solve  HP_solve_A HP_solve_I HP_solve_M
HP_solve_P  HP_solve_PA  HankelMatrix  Hausdorff  HenselLift  HermiteIntegrate  I  Is
JContinuedFraction  K KrullNumber  LE  LLFI_to_LPA  LLFPI_to_LPA LLF_to_LPA  LODO2FUN
LT  LUDecomp LUInverse  LUSolve LagrangeInterpolation  Lazard Lazard2  LazardQuotient
LazardQuotient2 LiePoly LiePolyIfCan  LowTriBddDenomInv LyndonBasis LyndonCoordinates
LyndonWordsList  LyndonWordsList1  MPtoMPT  NOT Not  Nul  OMParseError?  OMReadError?
OMUnknownCD?   OMUnknownSymbol?   OMbindTCP    OMclose   OMcloseConn   OMconnInDevice
OMconnOutDevice   OMconnectTCP   OMencodingBinary  OMencodingSGML   OMencodingUnknown
OMencodingXML   OMgetApp   OMgetAtp   OMgetAttr   OMgetBVar   OMgetBind   OMgetEndApp
OMgetEndAtp  OMgetEndAttr  OMgetEndBVar   OMgetEndBind  OMgetEndError  OMgetEndObject
OMgetError  OMgetFloat  OMgetInteger  OMgetObject OMgetString  OMgetSymbol  OMgetType
OMgetVariable  OMlistCDs OMlistSymbols  OMmakeConn  OMopenFile OMopenString  OMputApp
OMputAtp   OMputAttr  OMputBVar   OMputBind   OMputEndApp  OMputEndAtp   OMputEndAttr
OMputEndBVar   OMputEndBind   OMputEndError  OMputEndObject   OMputError   OMputFloat
OMputInteger  OMputObject  OMputString  OMputSymbol OMputVariable  OMread  OMreadFile
OMreadStr  OMreceive  OMsend  OMserve OMsetEncoding  OMsupportsCD?  OMsupportsSymbol?
OMunhandledSymbol   OMwrite   OR  One   Or   P   ParCond  ParCondList   Pfaffian   Pm
Pnan?  PollardSmallFactor  Pr   R1_to_R2_coercion  RF2UTS  ReduceOrder  RemainderList
RittWuCompare S  SEGMENT SFunction  SPDEnocancel1 STransform  STransform1 STransform2
ScanArabic ScanFloatIgnoreSpaces ScanFloatIgnoreSpacesIfCan ScanRoman Shi ShiftAction
ShiftC   Si   Somos    SturmHabicht   SturmHabichtCoefficients   SturmHabichtMultiple
SturmHabichtSequence   U   UP2UTS   UP2ifCan  UTS2UP   UnVectorise   UpTriBddDenomInv
VSUPI_to_VPA VSUPPI_to_VPA VSUPS_to_VPA Vectorise X  Y Zero aCubic aLinear aQuadratic
aQuartic abelianGroup abs absolutelyIrreducible? acos acosIfCan acosh acoshIfCan acot
acotIfCan  acoth  acothIfCan acsc  acscIfCan  acsch  acschIfCan adaptive  adaptive3D?
adaptive? addArrow! addArrows2Din2D addBadValue addChild! addMatch addMatchRestricted
addNode!  addObject!  addPlot1Din2D  addPlot1Din2Dparametric  addPlot1Din3Dparametric
addPlot2Din3D  addPlot2Din3Dparametric  addPoint  addPoint2  addPointLast  addPoints!
addSceneArrow  addSceneArrows  addSceneBox   addSceneClip  addSceneDef  addSceneGraph
addSceneGrid  addSceneGroup addSceneIFS  addSceneLine addSceneLines  addSceneMaterial
addSceneNamedPoints   addScenePattern    addSceneRuler   addSceneShape   addSceneText
addSceneTransform  addSceneUse  addWArrow!   addWarning  add_slots  addiag  additive?
addmod  adjacencyMatrix  adjoint  airyAi   airyAiPrime  airyBi  airyBiPrime  algDepHP
algDsolve   algSplitSimple  alg_reduce   alg_reduce0  alg_trial_division   algebraic?
algebraicCoefficients?  algebraicDecompose   algebraicGcd  algebraicOf  algebraicSort
algebraicVariables algint  algintegrate allDegrees allIndices  allRepeated allRootsOf
allSimpleCells alpha  alphaHilbert alphabetic alphabetic?  alphanumeric alphanumeric?
alternating   alternatingGroup  alternative?   analyseSymbol   and  anfactor   angerJ
annihilate?   ansatz  antiAssociative?   antiCommutative?  antiCommutator   anticoord
antipode   antisymmetric?   antisymmetricTensors    any   any?   append   appendPoint
appendRow!  apply   applyQuote  applyRules  applyTaylor   apply_taylor  approxNthRoot
approxSqrt approximants  approximate arbFunctions  arcsineDistribution areEquivalent?
arg1   arg2  argscript   argument   argumentList!   argumentListOf  arguments   arity
aromberg   arrayStack   arrowName    arrowsFromArrow   arrowsFromNode   arrowsToArrow
arrowsToNode  asec   asecIfCan  asech   asechIfCan  asimpson  asin   asinIfCan  asinh
asinhIfCan  assert  assign  assoc  associatedEquations  associatedSystem  associates?
associative?   associator   associatorDependence    atType   atan   atan1   atanIfCan
atanh   atanhIfCan  atom?   atoms   atrapezoidal   augment  autoReduce   autoReduced?
axes   axesColorDefault   back   backOldPos   badNum   badValues   balancedBinaryTree
balancedFactorisation   bandMatrix   bandedHessian    bandedJacobian   base   baseRDE
baseRDEsys  basicSet  basis  basisOfCenter  basisOfCentroid  basisOfCommutingElements
basisOfLeftAnnihilator  basisOfLeftNucleus   basisOfLeftNucloid  basisOfMiddleNucleus
basisOfNucleus   basisOfRightAnnihilator    basisOfRightNucleus   basisOfRightNucloid
bat   bat1  beauzamyBound   belong?   bernoulli  bernoulliB   bernoulliDistribution01
besselI  besselJ  besselK  besselY  bezoutDiscriminant  bezoutMatrix  bezoutResultant
biRank  binary  binaryFunction   binarySearchTree  binaryTournament  binaryTree  bind
binomThmExpt  binomial  bipolar  bipolarCylindrical   birth  bit?  bitCoef  bitLength
bitTruth  bits bivariate?  bivariatePolynomials  bivariateSLPEBR blankSeparate  block
blockConcat   blockSplit   blue   bombieriNorm   booleanConvolution   booleanCumulant
booleanCumulant2moment   booleanCumulantFromJacobi   booleanCumulants  bottom   bound
boundAt0 boundInf  boundOfCauchy boundary  box boxBoundary brace  bracket branchIfCan
branchPoint?  branchPointAtInfinity?   bright  brillhartIrreducible?  brillhartTrials
bringDown  bsolve btwFact  bubbleSort! build  bumprow bumptab  bumptab1 cAcos  cAcosh
cAcot  cAcoth  cAcsc  cAcsch  cAsec  cAsech cAsin  cAsinh  cAtan  cAtanh  cCos  cCosh
cCot  cCoth  cCsc  cCsch  cExp  cLog cPower  cRationalPower  cSec  cSech  cSin  cSinh
cTan  cTanh  calcRanges  call   canonicalBasis  canonicalIfCan  cap  car  cardinality
carmichaelLambda   cartesian   catalan   cdr  ceiling   center   certainlySubVariety?
chainSubResultants  changeBase  changeExprLength   changeVar  changeWeightLevel  char
charClass  character?   characteristic  characteristicPolynomial  characteristicSerie
characteristicSet   charlierC   charpol   charthRoot  chebyshevT   chebyshevU   check
checkExtraValues checkForZero  checkOptions checkRur checkType  check_sol1a chiSquare
chiSquare1  child  child?  children chineseRemainder  chinese_update  chkLibraryError
choosemon  chvar  class   classicalConvolution  classicalCumulant  classicalCumulants
clearCache  clearDenominator clearFortranOutputStack  clearTable! clearTheSymbolTable
clear_used_intrinsics clip  clipBoolean clipParametric  clipPointsDefault clipSurface
clipWithRanges  close  close!   closeComponent  closed?  closedCartesian  closedCurve
closedCurve?   closedTensor   cmult   coAdjoint   coHeight   code   coef   coefChoose
coefficient coefficientSet  coefficients coeffs0 coeffs1 coerce  coerceImages coerceL
coerceListOfPairs   coerceP  coercePreimagesImages   coerceS  coerceToType   colSlice
coleman  colinearity   collect  collectQuasiMonic  collectUnder   collectUpper  color
colorDef   colorFunction  column   columnMatrix  columnSpace   columns  commaSeparate
comment   common   commonDenominator  commutative?   commutativeEquality   commutator
comp   compBound   compactFraction   companionBlocks  comparison   compdegd   compile
compiledFunction  complement  complementSpace complementaryBasis  complete  complete2
completeEchelonBasis   completeEval   completeHensel  completeHermite   completeSmith
complex    complex?    complexEigenvalues    complexEigenvectors    complexElementary
complexExpand    complexForm     complexIntegrate    complexLimit    complexNormalize
complexNumeric complexNumericIfCan  complexRoots complexSolve  complexZeros component
components  compose  composite  composites  compound  computeBasis  computeCycleEntry
computeCycleLength computeInt computePowers concat  concat! cond condition conditionP
conditions   conditionsForIdempotents  conical   conj  conjug   conjugate  conjugates
connect  cons  consRow!  consnewpol   const  const?  constDsolve  constant  constant?
constantCoefficientRicDE  constantIfCan  constantKernel constantLeft  constantOpIfCan
constantOperator  constantRight  constantToUnaryFunction  construct  constructOrdered
contains?  containsPoint? content  continue continuedFraction  contraAdjoint contract
contractSolve   controlPanel  convergents   convert   coord  coordinate   coordinates
coordinatesIfCan  copies   coproduct  copy  copy!  copyInto!   copy_first  copy_slice
corrPoly  cos  cos2sec  cosIfCan  cosSinInfo cosh  cosh2sech  coshIfCan  cot  cot2tan
cot2trig  cotIfCan coth  coth2tanh coth2trigh  cothIfCan counit  count countRealRoots
countRealRootsMultiple    countable?     create    create3Space    createArrows2Din2D
createGenericMatrix        createIrreduciblePoly       createLowComplexityNormalBasis
createLowComplexityTable     createMultiplicationMatrix     createMultiplicationTable
createNormalElement   createNormalPoly   createNormalPrimitivePoly   createPlot1Din2D
createPlot1Din2Dparametric         createPlot1Din3Dparametric        createPlot2Din3D
createPlot2Din3Dparametric      createPrimitiveElement      createPrimitiveNormalPoly
createPrimitivePoly     createRandomElement    createSceneArrow     createSceneArrows
createSceneBox   createSceneClip   createSceneDef  createSceneGraph   createSceneGrid
createSceneGroup createSceneIFS  createSceneLine createSceneLines createSceneMaterial
createSceneNamedPoints     createScenePattern    createSceneRoot     createSceneRuler
createSceneShape createSceneText createSceneTransform createSceneUse createThreeSpace
createWidth   createX   createY   createZechTable  credPol   critB   critBonD   critM
critMTonD1  critMonD1 critT  critpOrder cross  crushedSet csc  csc2sin cscIfCan  csch
csch2sinh  cschIfCan   csubst  cubic  cumulant2moment  cup   currentSubProgram  curry
curryLeft  curryRight  curve  curve? curveColor  curveColorPalette  curveLoops  cycle
cycleClosed  cycleElt  cycleEntry  cycleLength cycleOpen  cyclePartition  cycleRagits
cycleSplit!  cycleTail cycles  cyclic cyclic?  cyclicCopy cyclicEntries  cyclicEqual?
cyclicGroup   cyclicParents    cyclicSubmodule   cyclotomic   cyclotomicDecomposition
cyclotomicFactorization  cylindrical cylindricalDecomposition  d  dAndcExp dP  dSubst
dU  dX  dark  datalist  ddFact   debug  debug3D  dec  decFatal  decXfFatal  decXfPass
decimal  declare   declare!  decompose  decomposeFunc   decreasePrecision  deductions
deepCopy   deepDiagramSvg   deepExpand  deepestInitial   deepestTail   defineProperty
definingEquations   definingInequation   definingPolynomial  degree   degreePartition
degreeSubResultant degreeSubResultantEuclidean  delay delete  delete! deleteProperty!
deleteRow!   delta  denom   denomLODE  denomRicDE   denominator  denominators   depth
dequeue   dequeue!  deref   deriv   derivationCoordinates  derivative   derivativeOf?
destruct  detSys   detSysNS  determinant   diag  diagonal   diagonal?  diagonalMatrix
diagonalProduct   diagonals   diagramHeight    diagramSvg   diagramWidth   dictionary
diff   diffHP   diffP   diffU   diffX   diff_map   difference   differentialVariables
differentials  differentiate  digamma  digit  digit?  digits  dihedral  dihedralGroup
dilate   dilog   dim   dimJ   dimS   dimension   dimensionOfIrreducibleRepresentation
dimensions   dimensionsOf   dioSolve    diophantineSystem   directProduct   directSum
directedGraph    direction    directions     directory    discreteLog    discriminant
discriminantEuclidean discriminantSet dispStatement  display displayKind displayLines
displayLines1  distFact  distance   distanceMatrix  distanceSquared  distanceWeighted
distdfact distribute  distributionByBooleanCumulants distributionByClassicalCumulants
distributionByEvenMoments  distributionByFreeCumulants distributionByJacobiParameters
distributionByMoments     distributionByMonotoneCumulants    distributionBySTransform
divergence   divide   divide!   divideExponents  divideIfCan   divideIfCan!   divisor
divisorCascade divisors  dmp2rfi dmpToHdmp  dmpToP doFactor  do_liou do_modular_solve
do_poly_integer   do_quo  do_with_error_env0   do_with_error_env1  do_with_error_env2
do_with_error_env3  dom  domainOf  dominantTerm  dot  double  double?  doubleComplex?
doubleDisc  doubleFloatFormat   doubleRank  doubleResultant   doublyTransitive?  draw
drawComplex drawComplexVectorField drawCurves  drawStyle drawToScale droot duplicates
duplicates?  e   eFromBinaryMap  ePseudoscalar  ee  ei_int   eigenMatrix  eigenvalues
eigenvector  eigenvectors  eisensteinIrreducible?  elColumn2! elRow1!  elRow2!  elem?
element   element?  elementary   elements   elimZeroCols!  ellipseBoundary   elliptic
ellipticCylindrical  ellipticE ellipticF  ellipticK ellipticPi  ellipticRC ellipticRD
ellipticRF  ellipticRJ elt  eltable? empty  empty? endOfFile?  endSubProgram enqueue!
enterInCache  enterPointData   entries  entry?  enumerate  epilogue   eq  eq?  eqRep?
equality  equation  erf  erfi  error  errorInfo  errorKind  escape  euclideanGroebner
euclideanNormalForm  euclideanSize euler  eulerE eulerPhi  eval eval1  eval1a eval_at
evaluate   evaluateInverse  even?   evenInfiniteProduct   evenlambert  every?   exQuo
exactQuotient exactQuotient! exists?  exp exp0 exp1 expIfCan  expPot expand expandLog
expandPower  expandTrigProducts  expextendedint   expint  expintegrate  expintfldpoly
explicitEntries? explicitlyEmpty? explicitlyFinite?  explogint explogs2trigs exponent
exponential exponential1  exponentialOrder exponents exprToGenUPS  exprToPS exprToUPS
exprToXXP  expr_to_series  expressIdealMember expression2Fortran  expression2Fortran1
exprex   expt  exptMod   exquo   extend   extendIfCan  extendToPoint   extendedCoords
extendedEuclidean    extendedIntegrate   extendedResultant    extendedSubResultantGcd
extended_gcd  extendedint  extension extensionDegree  exteriorDifferential  external?
externalList extract extract!  extractBottom! extractClosed extractIfCan extractIndex
extractPoint    extractProperty   extractSplittingLeaf    extractSymbol   extractTop!
eyeDistance   factor   factor1    factorAndSplit   factorByRecursion   factorFraction
factorGroebnerBasis   factorList  factorOfDegree   factorPolynomial  factorSFBRlcUnit
factorSquareFree  factorSquareFreeByRecursion   factorSquareFreePolynomial  factorial
factorials   factors   factorsOfCyclicGroupSize  factorsOfDegree   factorset   failed
failed?   false   ffactor   ffactor1   fffg  fglmIfCan   fibonacci   filename   fill!
fillPascalTriangle   filterUntil  filterWhile   find  findCycle   findNode  findPoint
finite?   finiteBasis   fintegrate   first  firstDenom   firstNumer   firstSubsetGray
firstUncouplingMatrix  firstn fixPredicate  fixedDivisor fixedPointExquo  fixedPoints
flagFactor   flatten  flexible?   flexibleArray  float   float?  floor   flush  fmecg
forLoop   formalDiff    formalDiff2   formula    fortFormatHead   fortFormatTypeLines
fort_clean_lines  fort_format_types  fortran  fortranCarriageReturn  fortranCharacter
fortranComplex   fortranDouble  fortranDoubleComplex   fortranInteger  fortranLiteral
fortranLiteralLine  fortranLogical  fortranReal   fortranTypeOf  fprindINFO  fracPart
fractRadix   fractRagits   fractionFreeGauss!  fractionPart   free?   freeConvolution
freeCumulant freeCumulant2moment  freeCumulants freeMultiplicativeConvolution freeOf?
freePoissonDistribution freeVariable?  fresnelC fresnelS  frobenius front  froot frst
fullDisplay  fullPartialFraction  function functionGraph  functionName  functionNames
gamma gauge  gaugeHilbert gaussianDistribution  gbasis gcd  gcdBasis gcdDecomposition
gcdPolynomial  gcdPrimitive gcdcofact  gcdcofactprim  gcdprim gderiv  genVectorStream
genVectorStream2   gen_Monte_Carlo_check  generalCoefficient   generalInfiniteProduct
generalInterpolation  generalLambert   generalPosition  generalSqFr  generalTwoFactor
generalizedContinuumHypothesisAssumed          generalizedContinuumHypothesisAssumed?
generalizedEigenvector  generalizedEigenvectors generalizedInverse  generateIrredPoly
generator       generators       generic       generic?       genericLeftDiscriminant
genericLeftMinimalPolynomial  genericLeftNorm  genericLeftTrace  genericLeftTraceForm
genericPosition         genericRightDiscriminant        genericRightMinimalPolynomial
genericRightNorm    genericRightTrace     genericRightTraceForm    genus    geometric
getArrowIndex  getArrows  getBadValues  getBoundValue getCentre  getChildren  getCode
getCurve   getDatabase   getEq   getGoodPrime   getGraph   getMatch   getMax   getMin
getMultiplicationMatrix getMultiplicationTable getName  getNames getNotation getOrder
getPickedPoints   getRef   getSimplifyDenomsFlag   getStatement   getStream   getType
getVariable getVariableOrder getVertexIndex  getVertices getZechTable get_fort_indent
get_rational_roots get_used_intrinsics get_variables gnuDraw goodPoint gotoJump grade
gradeInvolution  gradient  graeffe   gramschmidt  graphCurves  graphImage  graphState
graphStates  graphs green  groebSolve groebgen  groebner groebner?  groebnerFactorize
groebnerIdeal  ground   ground?  guess  guessADE  guessAlg   guessAlgDep  guessBinRat
guessExpRat   guessFE  guessHolo   guessPRec  guessPade   guessRat  guessRec   hMonic
hadamard  halfExtendedResultant1 halfExtendedResultant2  halfExtendedSubResultantGcd1
halfExtendedSubResultantGcd2  hankelDeterminant hankelDeterminants  hankelH1 hankelH2
harmonic  has?   hasDimension?  hasHi  hasPredicate?   hasSolution?  hasTopPredicate?
has_op?  hash   hashUpdate!  hasoln   hclf  hconcat   hcrf  hdmpToDmp   hdmpToP  head
headReduce headReduced?  headRemainder heap heapSort height  henselFact hensel_update
hermite  hermiteH hessian  hex  hexDigit hexDigit?  hi  high highCommonTerms  hilbert
hitherPlane   homogeneous  homogeneous?   horizConcat  horizJoin   horizSplit  hspace
htrigs  hue  hyperelliptic  hypergeometric0F1  hypergeometricF  iAiryAi  iAiryAiPrime
iAiryBi  iAiryBiPrime  iCompose  iExquo   iLambertW  id  id_map  ideal  idealSimplify
idealiser idealiserMatrix identification identity identityMatrix identitySquareMatrix
iexactQuo   ignore?   iiAiryAi   iiAiryAiPrime   iiAiryBi   iiAiryBiPrime   iiBesselI
iiBesselJ  iiBesselK  iiBesselY  iiBeta  iiGamma  iiHypergeometricF  iiPolylog  iiabs
iiacos  iiacosh  iiacot   iiacoth  iiacsc  iiacsch  iiasec   iiasech  iiasin  iiasinh
iiatan   iiatanh  iibinom   iicos  iicosh   iicot  iicoth   iicsc  iicsch   iidigamma
iidprod  iidsum  iiexp  iifact  iilog  iim2  iiperm  iipolygamma  iipow  iiretractVar
iisec  iisech iisin  iisinh  iisqrt2  iisqrt3 iitan  iitanh  imag  imagE imagI  imagJ
imagK  imagi imaginary  imagj  imagk implies  in?  inBounds? inDegree  inGroundField?
inHallBasis?   inR?  inRadical?   inc   incFail   incFatal  incLibraryError   incPass
incXfFail incXfFatal  incXfLibraryError incXfPass incidenceMatrix  inconsistent? incr
increasePrecision  increment  incrementBy incrementKthElement  indentFortLevel  index
index?  indexName  indexes   indiceSubResultant  indiceSubResultantEuclidean  indices
indicialEquation indicialEquationAtInfinity indicialEquations  inf infLex? infRittWu?
infieldIntegrate  infieldint infinite?  infiniteProduct  infinity infinityNorm  infix
infix? infsum  init initTable! initial  initializeGroupForWordProblem initiallyReduce
initiallyReduced?   initials   innerEigenvectors  innerSolve   innerSolve1   innerint
input  inrootof  insert  insert!  insertBottom!  insertMatch  insertRoot!  insertTop!
insertionSort!   inspect   int   intBasis  intChoose   intPatternMatch   intcompBasis
integ  integ_df  integer  integer?  integerBound  integerIfCan  integerPart  integers
integral   integral?    integralAtInfinity?   integralBasis   integralBasisAtInfinity
integralCoordinates integralDerivationMatrix  integralLastSubResultant integralMatrix
integralMatrixAtInfinity   integralRepresents   integrate  integrateIfCan   intensity
interReduce    internal?    internalAugment   internalDecompose    internalInfRittWu?
internalIntegrate   internalIntegrate0  internalLastSubResultant   internalSubPolSet?
internalSubQuasiComponent? internalZeroSetSplit interpolate interpret interpretString
intersect    intersection   interval    intoMatrix    inv   inverse    inverseColeman
inverseIntegralMatrix   inverseIntegralMatrixAtInfinity  inverseLaplace   invertIfCan
invertible?   invertibleElseSplit?    invertibleSet   invmod    invmultisect   iomode
ipow   iprint  iroot   irootDep  irreducible?   irreducibleFactor  irreducibleFactors
irreducibleRepresentation    is?    isAbsolutelyIrreducible?   isAcyclic?    isBasis?
isBoundNode?  isBox?  isCompound?  isDirectSuccessor? isDirected?  isEllipse?  isExpt
isFixPoint?  isFreeNode?  isFunctional?  isGreaterThan? isI?  isK?  isLambda?  isList
isMult isNodeBranch?  isNodeLeaf? isNull?  isOp isPlus isPoint?  isPointLeaf? isPower
isQuotient isS?  isTimes isVector? is_symbol? isobaric?  iter iteratedInitials jacobi
jacobi2poly jacobiCn  jacobiDn jacobiIdentity? jacobiMatrix  jacobiP jacobiParameters
jacobiPathArray  jacobiSn jacobiTheta  jacobiZeta jacobian  janko2 jetVariables  join
jordanAdmissible?  jordanAlgebra? karatsuba  karatsubaDivide karatsubaOnce  kelvinBei
kelvinBer kelvinKei kelvinKer kernel kernels  key key? keys kgraph kmax knownInfBasis
kovacic  kprod  kroneckerDelta  kroneckerProduct  kroneckerSum  kronecker_prod1  ksec
kummerM  kummerU  lSpaceBasis  label   lagrange  laguerre  laguerreL  lambda  lambert
lambertW  lambertW0  lambert_inverse_series  lambert_via_newton1  lambert_via_newton2
lambintegrate  landen  landen1  landen2  laplace  laplacian  laplacianMatrix  largest
last   lastSubResultant  lastSubResultantElseSplit   lastSubResultantEuclidean  latex
laurent  laurentIfCan  laurentRep  lazy?  lazyEvaluate  lazyGintegrate  lazyIntegrate
lazyIrreducibleFactors   lazyPquo   lazyPrem   lazyPremWithDefault   lazyPseudoDivide
lazyPseudoQuotient   lazyPseudoRemainder  lazyResidueClass   lazyVariations  lc   lcm
lcmCoef lcx0  lcz leader leadingBasisTerm  leadingCoefficient leadingCoefficientRicDE
leadingDer leadingExponent  leadingIdeal leadingIndex  leadingMonomial leadingSupport
leadingTerm    leaf?    leastAffineMultiple     leastMonomial    leastPower    leaves
left   leftAlternative?   leftCharacteristicPolynomial  leftDiscriminant   leftDivide
leftExactQuotient   leftExtendedGcd   leftFactor  leftFactorIfCan   leftGcd   leftLcm
leftMinimalPolynomial  leftMult  leftNorm  leftOne  leftPower  leftQuotient  leftRank
leftRankPolynomial leftRecip leftRegularRepresentation leftRemainder leftScalarTimes!
leftTrace  leftTraceMatrix leftTrim  leftUnit leftUnits  leftZero legendre  legendreP
legendreQ  length  lepol  lerchPhi   less?  level  leviCivitaSymbol  lex  lexGroebner
lexTriangular  lexico  lfextendedint  lfextlimint lfinfieldint  lfintegrate  lflogint
lfunc  lhs  li  li2  li_int  library  lie  lieAdmissible?  lieAlgebra?  lift  lifting
lifting1  light  lighting  limit   limitPart  limitPlus  limitedIntegrate  limitedint
linGenPos   linSolve   lin_sol    lineColorDefault   lineIntersect   linear   linear?
linearAssociatedExp   linearAssociatedLog    linearAssociatedOrder   linearDependence
linearDependenceOverConstants  linearDependenceOverZ  linearExtend  linearPolynomials
linearSearch     linearize     linearlyDependent?     linearlyDependentOverConstants?
linearlyDependentOverZ? linears  link linkToFortran  lintgcd list  list? listBranches
listConjugateBases listLoops listOfLists  listOfMonoms listOfTerms listRepresentation
listYoungTableaus   lists   lllip   lllp  llprop   lo   localAbs   localIntegralBasis
localReal?  localUnquote  log  log1   log10  log2  logDependenceQ  logGamma  logIfCan
logicF   logicT   logical?  logpart   lommelS1   lommelS2   lookup  loop   loopPoints
loopsArrows loopsAtNode  loopsNodes looseEquals  low lowerCase  lowerCase! lowerCase?
lowerPolynomial  lp lprop  lquo lyndon  lyndon? lyndonIfCan  m2r m_inverse  magnitude
mainCharacterization  mainCoefficients  mainContent  mainDefiningPolynomial  mainForm
mainKernel mainMonomial mainMonomials  mainPrimitivePart mainSquareFreePart mainValue
mainVariable  mainVariable? mainVariableOf  mainVariables  makeCell makeCos  makeCrit
makeEq   makeFEq  makeFR   makeFloatFunction   makeGraphImage  makeMulti   makeObject
makeRec  makeRecord  makeResult  makeSUP  makeSeries  makeSin  makeSketch  makeSystem
makeTerm   makeUnit  makeVariable   makeViewport2D  makeViewport3D   makeYoungTableau
makeop  makingStats?  mantissa  map  map!  mapBivariate  mapCoef  mapContra  mapDown!
mapExpon mapExponents mapGen mapMatrixIfCan mapSolve mapUnivariate mapUnivariateIfCan
mapUp!  mapall  mapdiv  mapmult  mask  match  match?  mathieu11  mathieu12  mathieu22
mathieu23 mathieu24 matrix matrixConcat3D  matrixDimensions matrixGcd max maxColIndex
maxDegree  maxDerivative  maxIndex   maxLevel  maxMixedDegree  maxPoints  maxPoints3D
maxPower maxRowIndex  maxShift maxSubst maxdeg maximumExponent  maxint maxrank maxrow
mdeg  meatAxe  medialSet  meijerG  meixnerM   member?  members  merge  merge!  merge2
mergeDifference  mergeFactors  merge_exponents  mesh  mesh?  meshFun2Var  meshPar1Var
meshPar2Var  message  messagePrint  middle   midpoint  midpoints  mightHaveRoots  min
minColIndex  minGbasis  minIndex  minPoints minPoints3D  minPol  minPoly  minRowIndex
mindeg mindegTerm  minimalPolynomial minimize minimumDegree  minimumExponent minordet
minrank   minset  minus!   minusInfinity  mirror   mix  mkAnswer   mkIntegral  mkPrim
mkcomm  modTree  modifyPoint  modifyPointData  modpeval  modpreduction  modularFactor
modularGcd  modularGcdPrimitive modularInvariantJ  module  moduleSum moduloP  modulus
moebius  moebiusMu  moment moment2Stransform  moment2booleanCumulant  moment2cumulant
moment2freeCumulant     moment2jacobi      moment2jacobi2     moment2monotoneCumulant
moment2nthJacobi    moments    monic?   monicCompleteDecompose    monicDecomposeIfCan
monicDivide   monicLeftDivide   monicModulo  monicRightDivide   monicRightFactorIfCan
monom  monomRDE  monomRDEsys  monomial  monomial?  monomialIntPoly  monomialIntegrate
monomials  monotoneConvolution   monotoneCumulant2moment  monotoneCumulant2momentPoly
monotoneCumulants  more? moreAlgebraic?  morphism  motzkinPathArray move  movedPoints
mpsode  mr  mrv_cmp  mrv_limit   mrv_limit1  mrv_normalize  mrv_rewrite  mrv_rewrite0
mrv_set mrv_sign  mul mul_by_binomial  mul_by_scalar mulmod  multMonom multiEuclidean
multiEuclideanTree  multiIndex  multi_SPDE  multifunctionGraph  multinomial  multiple
multiple? multiplicative?  multiplyCoefficients multiplyExponents  multisect multiset
multivariate   multivector    musserTrials   mvar    myDegree   naiveBeckermannLabahn
naiveBeckermannLabahn0     naiveBeckermannLabahn1     naiveBeckermannLabahnMultipoint
name   namedBranch   namedPoints   nand    nary?   ncDetSys   ncols   negative?   new
newFortranTempVar  newLine newReduc  newSubProgram newTypeLists  newline newton  next
nextColeman   nextIrreduciblePoly   nextItem  nextLatticePermutation   nextNormalPoly
nextNormalPrimitivePoly      nextPartition     nextPrime      nextPrimitiveNormalPoly
nextPrimitivePoly  nextSublist nextSubsetGray  next_sousResultant2 next_subResultant2
nil   nilFactor   nlde   noKaratsuba   noLinearFactor?   node   node?   nodeFromArrow
nodeFromNode   nodeOf?  nodeToArrow   nodeToNode   nodes  nonQsign   nonSingularModel
noncommutativeJordanAlgebra?   nor   norm  normDeriv2   normFactors   normInvertible?
normal   normal01   normal?    normalDenom   normalDeriv   normalElement   normalForm
normalise  normalisePoint  normalize normalizeAtInfinity  normalizeIfCan  normalized?
normalizedAssociate  normalizedDivide  not  notelem   npcoef  nrows  nsqfree  nthCoef
nthExpon  nthExponent   nthFactor  nthFlag  nthFractionalTerm   nthRoot  nthRootIfCan
nthr   null  null?   nullBoundary  nullSpace   nullary  nullary?   nullity  numDepVar
numFunEvals  numFunEvals3D  numIndVar   number?  numberOfChildren  numberOfComponents
numberOfComposites     numberOfComputedEntries    numberOfCycles     numberOfDivisors
numberOfFactors   numberOfFractionalTerms   numberOfHues   numberOfImproperPartitions
numberOfIrreduciblePoly  numberOfMonomials  numberOfNormalPoly  numberOfPrimitivePoly
numberOfVariables  numer  numerJP  numerator   numerators  numeric  numericIfCan  obj
objectOf   objects  oblateSpheroidal   octon   odd?  oddInfiniteProduct   oddintegers
oddlambert  ode  ode1 ode2  omError  omega  omegapower one  one?  oneDimensionalArray
op  opType   open  open?  operation   operator  operators  opeval   opposite?  option
option?  optional  optional? options  optpair  or  orbit  orbits ord  order  orderDim
ordinalAdd ordinalMul  ordinalPower orthogonalPolynomials  orthonormalBasis outDegree
outerProduct   outlineRender   output   outputArgs   outputAsFortran   outputAsScript
outputAsTex   outputFixed   outputFloating    outputForm   outputGeneral   outputList
outputSpacing  outputVRML  over overbar  overlabel  overlap  overset? pToDmp  pToHdmp
pack!  pack_exps  pack_exps0  pack_modulus   packageCall  pade  padecf  padicFraction
padicallyExpand  pair? palgLODE  palgLODE0  palgRDE  palgRDE0 palgextint  palgextint0
palgextintegrate   palginfieldint    palgint   palgint0    palgintegrate   palglimint
palglimint0   parabolic  parabolicCylindrical   paraboloidal  parallel   parametersOf
parametric?  paren parent  parse  parseIL parseIL2  parseILTerm parseLambda  parseSki
parseTerm  parseVar  parseVarTerm parse_integer  partialDenominators  partialFraction
partialNumerators partialQuotients particularSolution particularSolutionOverConstants
particularSolutionOverQ  partition  partitions  parts pascalTriangle  pastel  pattern
patternMatch patternMatchTimes  patternVariable pdct  perfectNthPower? perfectNthRoot
perfectSqrt   perfectSquare?  perm_to_vec   permanent  permutation   permutationGroup
permutationRepresentation    permutations    perpendicular    perspective    phiCoord
physicalLength  physicalLength!  pi  pile   pivot  pivots  plenaryPower  pleskenSplit
plot   plotPolar   plus   plus!  plusInfinity   pmComplexintegrate   pmintegrate   po
point  point?  pointColor  pointColorDefault  pointColorPalette  pointData  pointList
pointLists   pointPlot  pointSizeDefault   points  poisson   poissonDistribution  pol
polCase polar  polarCoordinates pole?  polyPart polyRDE polyRicDE  poly_int polygamma
polygon  polygon?  polylog  polynomial  polyred  pomopo!  pop!  popFortranOutputStack
position   position!   positive?    positivePower   positiveRemainder   positiveSolve
possibleOrder  possiblyInfinite?  possiblyNewVariety?   postfix  pow  powToUPS  power
power!  powerAssociative?  powerSum  powern   powers  powmod  pquo  pr2dmp  pre_gauss
pre_process  pre_smith precision  predicate  predicates  prefix prefix?  prefixRagits
prem  prepareDecompose  prepareSubResAlgo  presub  presuper  pretendOfType  prevPrime
previous  primPartElseUnitCanonical  primPartElseUnitCanonical!  primaryDecomp  prime
prime? primeFactor primeFrobenius primes primextendedint primextintfrac primintegrate
primintfldpoly    primitive?   primitiveElement    primitiveMonomials   primitivePart
primitivePart!   primitiveRowEchelon  primlogint   primlogintfrac  prinb   principal?
principalIdeal  principalSubResultantSet   prindINFO  prinpolINFO   prinshINFO  print
printCode  printHeader  printInfo   printInfo!  printStatement  printStats!  printSys
printTypes  printingInfo?  probablyZeroDim?   processTemplate  prod  product  project
projection projectionSet  prolateSpheroidal prologue prolong  prolongMV prolongSymbol
properties property proposition pseudoDivide pseudoQuotient pseudoRem pseudoRemainder
psolve ptFunc ptree  puiseux pureLex purelyAlgebraic? purelyAlgebraicLeadingMonomial?
purelyTranscendental?   purge!   push!  pushFortranOutputStack   pushdown   pushdterm
pushucoef  pushuconst   pushup  putColorInfo   putGraph  qPot   qShiftAction  qShiftC
qcoerce  qconvert   qcumulant2nthmoment  qelt  qfactor  qinterval   qlog  qnew  qroot
qsetelt!  qsetfirst! qsetrest!  qsqrt quadratic  quadraticForm quadraticNorm  quartic
quasiAlgebraicSet   quasiComponent  quasiMonic?   quasiMonicPolynomials  quasiRegular
quasiRegular?  quatern queue  quickSort  quo quoByVar  quote quoted?  quotedOperators
quotient   quotientByP   r2m   rCoord   rabs   radPoly   radical   radicalEigenvalues
radicalEigenvector     radicalEigenvectors    radicalOfLeftTraceForm     radicalRoots
radicalSimplify  radicalSolve  radix  raisePolynomial  ramified?  ramifiedAtInfinity?
ran   randnum  random   randomLC  randomR   range  rangePascalTriangle   ranges  rank
rarrow   ratDenom   ratDsolve   ratPoly  rational   rational?   rationalApproximation
rationalFunction    rationalIfCan    rationalPoint?   rationalPoints    rationalPower
rational_reconstruction  rationalize_ir  ratpart  ravel  rc  rdHack1  rdregime  read!
readIfCan!   readLine!   readLineIfCan!    readable?   real   real?   realEigenvalues
realEigenvectors   realElementary  realLiouvillian   realRoots  realSolve   realZeros
recip  reciprocalPolynomial recolor  reconstruct  rectangularMatrix  recur red  redPo
redPol   redmat   redpps   reduce  reduceBasis   reduceBasis0   reduceBasisAtInfinity
reduceByQuasiMonic    reduceLODE    reduceMod    reduced?    reducedContinuedFraction
reducedDiscriminant  reducedForm  reducedQPowers   reducedSystem  reduction  reductum
redux  ref   refine  regime   region  regularRepresentation   reindex  relationsIdeal
relativeApprox   relerror    rem   remainder   remainder!    remap_variables   remove
remove!  removeChild!  removeConstantTerm removeCosSq  removeCoshSq  removeDuplicates
removeDuplicates!       removeIrreducibleRedundantFactors      removeRedundantFactors
removeRedundantFactorsInContents      removeRedundantFactorsInPols     removeRepeats!
removeRoughlyRedundantFactorsInContents            removeRoughlyRedundantFactorsInPol
removeRoughlyRedundantFactorsInPols   removeSinSq   removeSinhSq   removeSquaresIfCan
removeSuperfluousCases   removeSuperfluousQuasiComponents   removeZero   removeZeroes
removeZeros     remove_denoms     rename     rename!    reopen!     reorder     repSq
repack1    repack_polys    repeatUntilLoop   repeatedIndex    repeating    repeating?
replace   replaceDiffs   replaceKthElement   representationType   represents   reseed
reset    resetBadValues    resetNew    resetVariableOrder   reshape    resize    rest
result    resultant   resultantEuclidean    resultantEuclidean_naif   resultantReduit
resultantReduitEuclidean    resultantSet    resultant_naif    retract    retractIfCan
retractable?   returnType!   returnTypeOf   returns   reverse   reverse!   reverseLex
revert       rewriteIdealWithHeadRemainder       rewriteIdealWithQuasiMonicGenerators
rewriteIdealWithRemainder                rewriteSetByReducingWithParticularGenerators
rewriteSetWithReduction  rhs ricDsolve  ridHack1 riemannZeta  right rightAlternative?
rightCharacteristicPolynomial   rightDiscriminant    rightDivide   rightExactQuotient
rightExtendedGcd    rightFactorCandidate     rightFactorIfCan    rightGcd    rightLcm
rightMinimalPolynomial   rightMult   rightNorm  rightOne   rightPower   rightQuotient
rightRank  rightRankPolynomial  rightRecip rightRegularRepresentation  rightRemainder
rightScalarTimes!   rightTrace   rightTraceMatrix  rightTrim   rightUnit   rightUnits
rightZero  rischDE  rischDEsys  rischNormalize   risch_de_ext  rk4  rk4a  rk4f  rk4qc
rmap   roman   romberg  rombergo   root   root?   rootBound  rootFactor   rootKerSimp
rootNormalize   rootOf    rootOfIrreduciblePoly   rootPoly    rootPower   rootProduct
rootRadius  rootSimp  rootSplit  rootSum   rootsOf  rotate  rotate!  rotatex  rotatey
rotatez  roughBase?  roughBasicSet roughEqualIdeals?  roughSubIdeal?  roughUnitIdeal?
round   routeArrowWeight   routeArrows    routeNodeWeight   routeNodes   row   rowEch
rowEchLocal   rowEchelon   rowEchelonLocal  rowMatrix   rowSlice   row_operation_base
row_operation_modular   rows   rquo  rroot   rspace   rst   rubiksGroup  rule   rules
ruleset  rur  sPol  safeCeiling  safeFloor  safety  safetyMargin  sample  samplePoint
satisfy?   saturate    save   say    sayLength   scalarMatrix    scalarTypeOf   scale
scaleRoots  scan  scanOneDimSubspaces  schema schwerpunkt  screenCoordX  screenCoordY
screenCoordZ  screenCoords   screenResolution  screenResolution3D   script  scripted?
scripts   se2rfi    search   sec   sec2cos   secIfCan    sech   sech2cosh   sechIfCan
second   seed  segment   select   select!  selectAndPolynomials   selectOrPolynomials
selectPolynomials      semiDegreeSubResultantEuclidean      semiDiscriminantEuclidean
semiIndiceSubResultantEuclidean semiLastSubResultantEuclidean semiResultantEuclidean1
semiResultantEuclidean2    semiResultantEuclidean_naif   semiResultantReduitEuclidean
semiSubResultantGcdEuclidean1     semiSubResultantGcdEuclidean2     semicolonSeparate
sendGraphImage separant  separate separateDegrees separateFactors  sequence sequences
series  seriesSolve  seriesToOutputForm  set setAdaptive  setAdaptive3D  setClipValue
setClosed setColumn! setCondition! setDifference setEmpty! setEpilogue! setErrorBound
setFieldInfo  setFormula!   setGcdMode  setImagSteps   setIntersection  setLabelValue
setLegalFortranSourceExtensions     setMaxPoints      setMaxPoints3D     setMinPoints
setMinPoints3D setMode setNotation setOfMinN  setOrder setOutMode setPoly setPosition
setPredicates   setPrologue!   setProperties  setProperty   setRealSteps   setRedMode
setRow!  setScreenResolution setScreenResolution3D  setSimpMode setSimplifyDenomsFlag
setStatus  setStatus!   setTex!  setTopPredicate  setTransform!   setUnion  setValue!
setVariableOrder   setchildren!  setelt!   setfirst!  setlast!   setleaves!  setleft!
setnext!  setprevious! setref  setrest!  setright! setsubMatrix!  setvalue! sh  shade
shallowCopy  shallowExpand shanksDiscLogAlgorithm  shellSort shift  shiftHP shiftLeft
shiftRight  shiftRoots show  showAll? showAllElements  showArrayValues showClipRegion
showElements  showFortranOutputStack  showRegion showScalarValues  showTheSymbolTable
showTypeInOutput  shrinkable  shuffle   shufflein  sierpinskiDivide  sign  signAround
simpMod   simpOne  simpleCells   simplify   simplifyCoeffs  simplifyExp   simplifyLog
simplifyPower simpson simpsono sin sin2csc  sin? sinIfCan sincos singRicDE singleFace
singleFactorBound  singular? singularAtInfinity?  sinh  sinh2csch sinhIfCan  sinhcosh
sipnt sivec size size?  sizeLess? sizeMultiplication sizePascalTriangle skewSFunction
ski slash  slex smaller? smesh  smith sn2 sncndn  solid solid? solve  solve1 solveFor
solveInField   solveLinear   solveLinearOverConstants   solveLinearPolynomialEquation
solveLinearPolynomialEquationByFractions     solveLinearPolynomialEquationByRecursion
solveLinearlyOverQ  solveRetract   solve_u  solveid   someBasis  sort   sort!  sortLD
sorted? sortedPurge!  space spanningForestArrow  spanningForestNode spanningTreeArrow
spanningTreeNode  specialTrigs   specialise  spherical  split   split!  splitConstant
splitDenominator   splitNodeOf!   splitSquarefree   spnt   sqfrFactor   sqfree   sqrt
square?   squareFree    squareFreeBasis   squareFreeFactors   squareFreeLexTriangular
squareFreePart  squareFreePolynomial  squareFreePrim squareMatrix  squareTop  stFunc1
stFunc2  stFuncN  stack  standardBasisOfCyclicSubmodule  startPolynomial  startStats!
startTable!  startTableGcd!  startTableInvSet!  statement2Fortran  statistics  status
stirling   stirling1  stirling2   stop   stopMusserTrials  stopTable!   stopTableGcd!
stopTableInvSet!      stoseIntegralLastSubResultant     stoseInternalLastSubResultant
stoseInvertible?   stoseInvertible?_reg  stoseInvertible?_sqfreg   stoseInvertibleSet
stoseInvertibleSet_reg         stoseInvertibleSet_sqfreg        stoseLastSubResultant
stosePrepareSubResAlgo  stoseSquareFreePart   stransform  stranslate   stream  string
string?   stripCommentsAndBlanks  strongGenerators   stronglyReduce  stronglyReduced?
structuralConstants  struveH  struveL  stube  sturmSequence  sturmVariationsOf  style
sub   subCase?   subMatrix    subNode?   subNodeOf?   subPolSet?   subQuasiComponent?
subResultantChain subResultantGcd  subResultantGcdEuclidean subResultantsChain subSet
subTriSet?  subdiagramSvg  subdivide submod  subresultantSequence  subresultantVector
subscript   subset?   subspace   subspace?  subst   substHP   substitute   substring?
subtractIfCan  suchThat  suffix?  sum  sumBasis  sumOfDivisors  sumOfKthPowerDivisors
sumSquares  summation  sunion  sup  supDimElseRittWu?  supRittWu?  super  superscript
supersub  support  surface svec  swap  swap!  swapColumns! swapRows!  sylvesterMatrix
sylvesterSequence  symFunc  symbol   symbol?  symbolIfCan  symbolTable  symbolTableOf
symmetric?   symmetricDifference   symmetricGroup   symmetricPower   symmetricProduct
symmetricRemainder  symmetricSquare symmetricTensors  systemCommand t  tRange tValues
tab tab1 table  tableForDiscreteLogarithm tablePow tableau tail  tan tan2cot tan2trig
tanAn tanIfCan  tanNa tanQ  tanSum tanh  tanh2coth tanh2trigh  tanhIfCan tanintegrate
taylor taylorIfCan  taylorQuoByVar taylorRep taylor_via_deriv  taylor_via_lode tensor
tensorMap tensorProduct  terminal terms test  testAbsolutePrecision testComplexEquals
testComplexEqualsAux     testDim     testEquals    testEqualsAux     testEqualsAuxCmp
testLibraryError   testLibraryErrorAux  testModulus   testNotEquals  testNotEqualsAux
testRealEquals testRealEqualsAux testRelativePrecision  testTrue testTrueAux testcase
testcaseNoClear testsuite  testsuiteNoClear tex  thetaCoord third times  times! title
toCayleyGraph  toObj  toPermutation  toPoint toSVG  toScale  toString  toStringConven
toStringUnwrapped   toTable  toVector   toX3D  to_mod_pa   top  topFortranOutputStack
topPredicate   toroidal  torsion?   torsionIfCan  toseInvertible?   toseInvertibleSet
toseLastSubResultant      toseSquareFreePart       totalDegree      totalDegreeSorted
totalDifferential totalGroebner totalLex totalfract  totolex tower trace trace2PowMod
traceMatrix   tracePowMod   trailingCoefficient   transcendenceDegree   transcendent?
transcendentalDecompose  transform   translate  transpose   trapezoidal  trapezoidalo
traverse   tree   triangSolve   triangular?   triangularSystems   triangulate   trigs
trigs2explogs  trim trivialIdeal?  true  trueEqual  trunc truncate  truncated_mul_add
truncated_multiplication    tryFunctionalDecomposition    tryFunctionalDecomposition?
tryTwoFactor tube tubePlot  tubePoints tubePointsDefault tubeRadius tubeRadiusDefault
twist   twoFactor    type   typeList   typeLists   ucodeToString    uentries   unary?
unaryFunction  unbind uncouplingMatrices  undirectedGraph unexpand  uniform uniform01
union   unit   unit?   unitCanonical  unitNormal   unitNormalize   unitVector   units
unitsColorDefault  univariate univariate?  univariatePolynomial univariatePolynomials
univariatePolynomialsGcds     univariateSolve     univcase     universe     unmakeSUP
unpack_poly   unparse   unprotectedRemoveRedundantFactors   unrankImproperPartitions0
unrankImproperPartitions1   unravel    untab   unvectorise    upDateBranches   updatD
updatF    update    update!    updateStatus!    upperCase    upperCase!    upperCase?
useEisensteinCriterion  useEisensteinCriterion? useNagFunctions  useSingleFactorBound
useSingleFactorBound?   userOrdered?   usingTable?  validExponential   value   values
var   var1Steps   var1StepsDefault   var2Steps  var2StepsDefault   varList   variable
variable?   variableName  variableOf   variables  variablesOf   variationOfParameters
vark   varselect   vconcat   vector   vector_add_mul   vector_combination   vectorise
vertConcat vertSplit viewDefaults  viewDeltaXDefault viewDeltaYDefault viewPhiDefault
viewPosDefault  viewSizeDefault viewThetaDefault  viewWriteAvailable viewWriteDefault
viewZoomDefault viewpoint viewport2D viewport3D  virtualDegree void vspace weakBiRank
weberE   weierstrass    weierstrassHalfPeriods   weierstrassInvariants   weierstrassP
weierstrassP0 weierstrassPPrime weierstrassPPrime0 weierstrassSigma weierstrassSigma0
weierstrassZeta     weierstrassZeta0    weight     weighted    weightedDistanceMatrix
weightedGraph   weights  whatInfinity   whileLoop  whittakerM   whittakerW  wholePart
wholeRadix  wholeRagits  width   wignerDistribution  withPredicates  wordInGenerators
wordInStrongGenerators   wordsForStrongGenerators  wreath   writable?  write   write!
writeCategory writeLine!  writeObj writePackage writeSvg  writeSvgQuantised writeVRML
writeX3d writeXml  wronskianMatrix wrregime  xCoord xRange  xform xftestComplexEquals
xftestComplexEqualsAux      xftestEquals      xftestEqualsAux      xftestLibraryError
xftestLibraryErrorAux     xftestNotEquals     xftestNotEqualsAux     xftestRealEquals
xftestRealEqualsAux xftestTrue  xftestTrueAux xmlAttribute  xmlElement xn  xor yCoord
yCoordinates  yRange  yellow  youngGroup  zCoord   zRange  zag  zero  zero?  zeroDim?
zeroDimPrimary?   zeroDimPrime?  zeroDimensional?   zeroMatrix  zeroOf   zeroSetSplit
zeroSetSplitIntoTriangularSystems  zeroSquareMatrix  zeroVector   zerosOf  zeta  zoom
%e  %i   %infinity  %minusInfinity  %pi   %plusInfinity  OneDimensionalArrayAggregate
AbelianGroup             AbelianSemiGroup            AlgebraicallyClosedFunctionSpace
PlaneAlgebraicCurvePlot  Algebra   AlgebraGivenByStructuralConstants  AssociationList
AlgebraicNumber     AntiSymm      TwoDimensionalArrayCategory     TwoDimensionalArray
ArcTrigonometricFunctionCategory            Automorphism           BalancedBinaryTree
BinaryExpansion    Bits    BasicOperator    BalancedPAdicRational    BinarySearchTree
BinaryTreeCategory      BinaryTree      CartesianTensor      ComplexDoubleFloatMatrix
Cell       Collection      Color       ComplexCategory      SubSpaceComponentProperty
Database     Dequeue     DoubleFloat    DoubleFloatVector     DenavitHartenbergMatrix
DifferentialExtension      DictionaryOperations       DirectProduct      Distribution
DistributedJetBundlePolynomial       DataList       DistributedMultivariatePolynomial
DirectProductMatrixModule      DirectProductModule     DifferentialPolynomialCategory
DrawOption    DifferentialSparseMultivariatePolynomial   DifferentialVariableCategory
ExtAlgBasis                       ElementaryFunctionsGeneralizedUnivariatePowerSeries
ElementaryFunctionsUnivariateLaurentSeries ElementaryFunctionsUnivariatePuiseuxSeries
ExtensibleLinearAggregate         EltableAggregate         EntireRing         EqTable
EuclideanDomain       Exit      Expression       ExponentialOfUnivariatePuiseuxSeries
FreeAbelianGroup         FiniteAbelianMonoidRing        FiniteAlgebraicExtensionField
FortranCode           FiniteDivisor           FullyEvalableOver           FiniteField
FiniteFieldCyclicGroup                    FiniteFieldCyclicGroupExtensionByPolynomial
FiniteFieldCyclicGroupExtension       FiniteFieldCategory      FiniteFieldNormalBasis
FiniteFieldNormalBasisExtensionByPolynomial           FiniteFieldNormalBasisExtension
FiniteFieldExtensionByPolynomial  FiniteFieldExtension  FreeGroup  FiniteGraph  Field
File  FiniteRankNonAssociativeAlgebra Finite  FiniteRankAlgebra FiniteLinearAggregate
FullyLinearlyExplicitRingOver  Float   FreeModuleCategory  FileName  FreeNilpotentLie
FortranFormat      FullPartialFractionExpansion     FloatingPointSystem      Fraction
FullyRetractableTo     FramedModule    FunctionSpace     FourierSeries    FortranType
FunctionCalled  GenericNonAssociativeAlgebra GeneralDistributedMultivariatePolynomial
GeneralizedUnivariatePowerSeries     GeneralModulePolynomial    GuessOptionFunctions0
GeneralPolynomialSet  GraphImage Group  GeneralSparseTable  Pi HashTable  GuessOption
MaybeSkewPolynomialCategory  GradedAlgebra GradedModule  GeneralUnivariatePowerSeries
GeneralTriangularSet      HashState      HomogeneousDistributedMultivariatePolynomial
HomogeneousDirectProduct        HyperellipticFiniteDivisor       HomogeneousAggregate
HyperbolicFunctionCategory  IndexedOneDimensionalArray   IndexedBits  PolynomialIdeal
InnerEvalable  IndexedFlexibleArray InnerIndexedTwoDimensionalArray  IndexedJetBundle
ILogic    IndexedExponents     IntegerNumberSystem    InnerTable    InnerPAdicInteger
IntegrationResult  InnerSparseUnivariatePowerSeries  InnerTaylorSeries  IndexedVector
JetBundleBaseFunctionCategory       JetBundleCategory       JetBundleFunctionCategory
JetBundlePolynomial      JetBundle       JetDifferentialEquation      JetLazyFunction
JetVectorField      KeyedDictionary      LocalAlgebra     Lambda      LieExponentials
AssociatedLieAlgebra  List ListMonoidOps  Localize LinearOrdinaryDifferentialOperator
LinearOrdinaryDifferentialOperator1               LinearOrdinaryDifferentialOperator2
LinearOrdinaryDifferentialOperatorCategory   Logic    LiePolynomial   LieSquareMatrix
LazyStreamAggregate   ModularAlgebraicGcdTools2   Magma   Matrix   MultifunctionGraph
MachineInteger     MathMLFormat     ModMonic      ModuleOperator     Module     Monad
MonogenicAlgebra  MultivariatePolynomial   Multiset  MultivariateTaylorSeriesCategory
NonAssociativeAlgebra NonAssociativeRng NonAssociativeRing Enumeration ping Record on
NonNegativeInteger None NewSparseMultivariatePolynomial NewSparseUnivariatePolynomial
OctonionCategory    Octonion    OrderedDirectProduct    OrderlyDifferentialPolynomial
OrdinaryDifferentialRing       OrderedExpression        OpenMath       OpenMathDevice
OpenMathError  OppositeMonogenicLinearOperator  OnePointCompletion  OrderedCompletion
OrderedSet       UnivariateSkewPolynomialCategory      SparseUnivariateSkewPolynomial
UnivariateSkewPolynomial OutputForm OrdinaryWeightedPolynomials PAdicRational Palette
ParametricSpaceCurve  PatternMatchListResult Pattern  PoincareBirkhoffWittLyndonBasis
PartialDifferentialOperator                PendantTree               PermutationGroup
PolynomialFactorizationExplicit   PartialFraction   Plot   Point   PolynomialCategory
PolynomialRing  Product  PowerSeriesCategory QuasiAlgebraicSet  QuotientFieldCategory
Quaternion   QuaternionCategory    RadicalCategory   RadixExpansion   RealClosedField
Reference  ResidueRing  RetractableFrom  RegularChain  RectangularMatrixCategory  Rng
RightOpenIntervalRootCharacterization  RomanNumeral  RealRootCharacterizationCategory
RegularTriangularSetCategory      RewriteRule      Ruleset      SingletonAsOrderedSet
SBoundary   Scene    SceneNamedPoints   SCartesian   SequentialDifferentialPolynomial
SequentialDifferentialVariable         Segment        SparseEchelonMatrix         Set
SetCategory     SExpression    SimpleFortranProgram     SplitHomogeneousDirectProduct
SingleInteger          SparseMultivariateSkewPolynomial          SquareMatrixCategory
SparseMultivariatePolynomialExpressions  SparseMultivariateTaylorSeries  SmallOrdinal
ThreeSpace    SplittingTree     SquareMatrix    SortedExponentVector    SplittingNode
SPointCategory       StringAggregate       SquareFreeRegularTriangularSet       Stack
SparseTable     Stream     StringTable    SuchThat     StreamAggregate     STransform
String     SubSpace      SparseUnivariateLaurentSeries     SparseUnivariatePolynomial
SparseUnivariatePuiseuxSeries   SparseUnivariateTaylorSeries  Symbol   TheSymbolTable
Table    TableAggregate    TensorPowerCategory   TexFormat    TexmacsFormat    Switch
SymmetricPolynomial   SymbolTable   Tableau    TensorProduct   TensorPower   TextFile
TranscendentalFunctionCategory   Tree    TrigonometricFunctionCategory   TaylorSeries
TaylorSeriesExpansion  TaylorSeriesExpansionGeneralized  TaylorSeriesExpansionLaurent
TaylorSeriesExpansionPuiseux      TaylorSeriesExpansionTaylor     TubePlot      Typed
U16Vector   U32Vector   U8Vector  UniqueFactorizationDomain   UnivariateLaurentSeries
TriangularSetCategory    Tuple   U16Matrix    U32Matrix   U8Matrix    UndirectedGraph
UnivariateFormalPowerSeries                UnivariateLaurentSeriesConstructorCategory
UnivariateLaurentSeriesConstructor   UniversalSegment  Untyped   UnivariatePolynomial
UnivariatePolynomialCategory   UnivariatePowerSeriesCategory  UnivariatePuiseuxSeries
UnivariatePuiseuxSeriesConstructorCategory         UnivariatePuiseuxSeriesConstructor
UnivariatePuiseuxSeriesWithExponentialSingularity             UnaryRecursiveAggregate
UnivariateTaylorSeries             UnivariateTaylorSeriesCategory            Variable
VectorIntegerReconstructor    Vector     ThreeDimensionalViewport    VectorSpaceBasis
WeightedGraph        WuWenTsunTriangularSet        ExtensionField        XmlAttribute
XPBWPolynomial          XPolynomialRing         IntegerMod          AlgebraicFunction
AlgebraicManipulations   AlgebraPackage    ApplyUnivariateSkewPolynomial   ApplyRules
OneDimensionalArrayFunctions2    TwoDimensionalArrayFunctions   BalancedFactorisation
BasicOperatorFunctions1    BrillhartTests    CylindricalAlgebraicDecompositionPackage
CylindricalAlgebraicDecompositionUtilities                  CartesianTensorFunctions2
CommonDenominator           CharacteristicPolynomialPackage          ChangeOfVariable
ComplexIntegerSolveLinearPolynomialEquation  CartanKuranishi ConstantLinearDependence
TwoDimensionalPlotClipping          ComplexRootPackage          CombinatorialFunction
IntegerCombinatoricFunctions    CommonOperators   CommuteUnivariatePolynomialCategory
compCode         ComplexFactorization        ComplexFunctions2         ComplexPattern
compUtil        CoordinateSystems        CharacteristicPolynomialInMonogenicalAlgebra
ComplexPatternMatch     CRApackage    ComplexRootFindingPackage     CyclicStreamTools
ComplexTrigonometricManipulations      CoerceVectorMatrixPackage      CycleIndicators
CyclotomicPolynomialPackage       DoubleResultantPackage      DistinctDegreeFactorize
ElementaryFunctionDefiniteIntegration             RationalFunctionDefiniteIntegration
DegreeReductionPackage      DefiniteIntegrationTools      DoubleFloatSpecialFunctions
DoubleFloatSpecialFunctions2    DiophantineSolutionPackage    DirectProductFunctions2
DisplayPackage            DistributionPackage           DistributionPolynomialPackage
DistributionFunctions2         DiscreteLogarithmPackage         TopLevelDrawFunctions
TopLevelDrawFunctionsForCompiledFunctions     TopLevelDrawFunctionsForAlgebraicCurves
DrawComplex   DrawNumericHack   TopLevelDrawFunctionsForPoints   DrawOptionFunctions0
DrawOptionFunctions1      DistributionContinuedFractionPackage     ElementaryFunction
ElementaryFunctionStructurePackage            EllipticFunctionsUnivariateTaylorSeries
ExpressionLinearSolve      ELIPIDF     DoubleFloatEllipticIntegrals      EigenPackage
EquationFunctions2              ErrorFunctions              ExpressionSpaceFunctions1
ExpressionSpaceFunctions2               EvaluateCycleIndicators              Export3D
ExpressionFunctions2    ExpressionToUnivariatePowerSeries    ExpressionSpaceODESolver
ExpressionSolve      ExpressionTubePlot     FactoredFunctions      FactoringUtilities
FiniteAbelianMonoidRingFunctions2         FortranCodePackage1        FortranCodeTools
FiniteDivisorFunctions2     FloatEllipticFunctions    FunctionFieldCategoryFunctions2
FiniteFieldFunctions    FractionFreeFastGaussian    FractionFreeFastGaussianFractions
FiniteFieldHomomorphisms    FunctionFieldIntegralBasis   FiniteFieldPolynomialPackage
FiniteFieldPolynomialPackage2                FiniteFieldSolveLinearPolynomialEquation
FGLMIfCanPackage       FiniteLinearAggregateFunctions2      FiniteLinearAggregateSort
FloatLiouvilianFunctions          FloatingComplexPackage          FloatingRealPackage
FreeModuleFunctions2             FortranOutputStackPackage            FindOrderFinite
ScriptFormulaFormat1                 FortranPackage                FactoredFunctions2
FractionFunctions2   FractionalIdealFunctions2  FramedNonAssociativeAlgebraFunctions2
FactoredFunctionUtilities FunctionSpaceFunctions2 FunctionSpaceToExponentialExpansion
FunctionSpaceToUnivariatePowerSeries            FunctionSpaceToUnivariatePowerSeries2
FiniteSetAggregateFunctions2                          FunctionSpaceComplexIntegration
FloatSpecialFunctions        FunctionSpaceIntegration       FunctionalSpecialFunction
FunctionSpacePrimitiveElement      FunctionSpaceReduce     FunctionSpaceRationalRoots
FunctionSpaceUnivariatePolynomialFactor                         GaloisGroupFactorizer
GaloisGroupFactorizationUtilities GaloisGroupPolynomialUtilities GaloisGroupUtilities
GaussianFactorizationPackage       GroebnerPackage      EuclideanGroebnerBasisPackage
GroebnerFactorizationPackage      GroebnerInternalPackage       GcdBasis      GnuDraw
GenExEuclid        GeneralizedMultivariateFactorize       GeneralPolynomialGcdPackage
GenUFactorize            GenerateUnivariatePowerSeries           GeneralHenselPackage
GrayCode           GroebnerSolve           GuessAlgebraicNumber           GuessFinite
GuessInteger          GuessPolynomialFunctions          HankelPackage          HeuGcd
ChineseRemainderToolsForIntegralBases    IntegralBasisTools    InnerCommonDenominator
InnerMatrixLinearAlgebraFunctions                   InnerMatrixQuotientFieldFunctions
InnerModularHermitePade        InnerNormalBasisFieldFunctions        IncrementingMaps
Infinity        InfiniteProductCharacteristicZero       InnerNumericFloatSolvePackage
InnerModularGcd    InfiniteProductFiniteField     InnerPolySign    AlgebraicIntegrate
DenominatorIntegration    IntegerFactorizationPackage    IntegerNumberTheoryFunctions
TranscendentalHermiteIntegration     PureAlgebraicIntegration     RationalIntegration
RationalFunctionIntegration                      IntegerSolveLinearPolynomialEquation
IntegrationTools          InverseLaplaceTransform         IntegrationResultFunctions2
IntegerRoots             IntegrationResultRFToFunction            IrrRepSymNatPackage
InternalRationalUnivariateRepresentationPackage                IntegerSmithNormalForm
InfiniteTupleFunctions2  InnerTrigonometricManipulations  JetCoordinateTransformation
KernelFunctions2   LaplaceTransform    LeadingCoefDetermination   LiouvillianFunction
PowerSeriesLimitPackage   LinearDependence   ListToMap   ElementaryFunctionLODESolver
InnerPolySum  InfiniteTupleFunctions3   JetGroebner  Kovacic  LazardSetSolvingPackage
LexTriangularPackage          LinGroebnerPackage         RationalFunctionLimitPackage
ListFunctions2       ListFunctions3      LinearOrdinaryDifferentialOperatorFactorizer
LinearOrdinaryDifferentialOperatorsOps            LinearPolynomialEquationByFractions
LinearSystemMatrixPackage   LinearSystemMatrixPackage1  LinearSystemPolynomialPackage
LUDecomposition  ModularAlgebraicGcd MatrixManipulation  MappingPackageInternalHacks1
MappingPackageInternalHacks2       MappingPackageInternalHacks3       MappingPackage1
MappingPackage2 MappingPackage3 MatrixCategoryFunctions2 MatrixLinearAlgebraFunctions
StorageEfficientMatrixOperations                       MultiVariableCalculusFunctions
MatrixCommonDenominator                               ModularDistinctDegreeFactorizer
MeshCreationRoutinesForThreeDimensions MultFiniteFactorize MakeBinaryCompiledFunction
MakeFunction    MakeUnaryCompiledFunction     MultipleMap    ModularHermitePadeSolver
MonomialExtensionTools      MPolyCatFunctions3     MPolyCatRationalFunctionFactorizer
MRationalFactorize       MrvLimitPackage       MergeThing       MultivariateFactorize
NaiveBeckermannLabahnModular  NumericContinuedFraction NonCommutativeOperatorDivision
NewtonInterpolation           NGroebnerPackage           NonLinearFirstOrderODESolver
NormInMonogenicAlgebra           NormRetractPackage           NumericRealEigenPackage
NewSparseUnivariatePolynomialFunctions2            NumberTheoreticPolynomialFunctions
Numeric              NumberFormats             NumericalOrdinaryDifferentialEquations
NumericalQuadrature    NumericTubePlot     OctonionCategoryFunctions2    ConstantLODE
ElementaryFunctionODESolver    ODEIntegration     PureAlgebraicLODE    PrimitiveRatDE
PrimitiveRatRicDE     RationalLODE    ReduceLODE     RationalRicDE    SystemODESolver
ODETools         OutputFormTools         ExpressionToOpenMath         OpenMathPackage
OpenMathServerPackage           OnePointCompletionFunctions2          OperationsQuery
OrderedCompletionFunctions2   OrderingFunctions   UnivariateSkewPolynomialCategoryOps
OrthogonalPolynomialFunctions              OutputPackage             PadeApproximants
PadeApproximantPackage     PolynomialAN2Expression     ParametricPlaneCurveFunctions2
PathArrayPackage      ParametricSpaceCurveFunctions2      ParametricSurfaceFunctions2
PartitionsAndPermutations          PatternMatch          PatternMatchResultFunctions2
PatternFunctions1  PatternFunctions2   PolynomialComposition  PolynomialDecomposition
PartialDifferentialOperatorHelper       Permanent       PolynomialEvaluationUtilities
PolynomialFactorizationByRecursion       PolynomialFactorizationByRecursionUnivariate
PointsOfFiniteOrder        PointsOfFiniteOrderRational       PointsOfFiniteOrderTools
PartialFractionPackage          PartialFractionUtilities         PolynomialGcdPackage
PermutationGroupExamples     PolyGroebner     PiCoercions     PolynomialInterpolation
PolynomialInterpolationAlgorithms  ParallelIntegrationTools ParametricLinearEquations
PlotFunctions1      PlotTools     PatternMatchAssertions      FunctionSpaceAssertions
PatternMatchPushDown     PatternMatchFunctionSpace    PatternMatchIntegerNumberSystem
PatternMatchKernel      PatternMatchListAggregate      PatternMatchPolynomialCategory
AttachPredicates    FunctionSpaceAttachPredicates   PatternMatchQuotientFieldCategory
PatternMatchSymbol    PatternMatchTools   PolynomialNumberTheoryFunctions    PolToPol
RealPolynomialUtilitiesPackage  PolynomialFunctions2 PolynomialToUnivariatePolynomial
PolynomialCategoryQuotientFunctions     PolynomialCategoryLifting     PolynomialRoots
U32VectorPolynomialOperations          PrecomputedAssociatedEquations         PrimGCD
PrimitiveArrayFunctions2    PrimitiveElement     IntegerPrimesPackage    PrintPackage
PseudoRemainderSequence      PolynomialSetUtilitiesPackage     PseudoLinearNormalForm
PolynomialSquareFree        PointFunctions2         PointPackage        PushVariables
PAdicWildFunctionFieldIntegralBasis      QuasiAlgebraicSet2     QuasiComponentPackage
QuotientFieldCategoryFunctions2    QuaternionCategoryFunctions2    RandomNumberSource
RationalRetractions   ElementaryRischDE    ElementaryRischDEX   TranscendentalRischDE
RandomDistributions      RealZeroPackage       RealSolvePackage      ReductionOfOrder
RepresentationPackage1  RepeatedDoubling   ResolveLatticeCompletion  RationalFunction
RationalFunctionFactor RandomIntegerDistributions RectangularMatrixCategoryFunctions2
RegularSetDecompositionPackage                         RegularTriangularSetGcdPackage
RationalUnivariateRepresentationPackage             SimpleAlgebraicExtensionAlgFactor
SAERationalFunctionAlgFactor SortedCache ScanningUtilities StructuralConstantsPackage
SegmentFunctions2             SegmentBindingFunctions2             SequenceFunctions2
SquareFreeQuasiComponentPackage              SquareFreeRegularTriangularSetGcdPackage
SymmetricGroupCombinatoricFunctions     SturmHabichtPackage    ElementaryFunctionSign
RationalFunctionSign       SimplifyAlgebraicNumberConvertPackage      SmithNormalForm
SparsePolynomialCoercionHelpers     PolynomialSolveByFormulas     RadicalSolvePackage
TransSolvePackageService    TransSolvePackage     SortPackage    SpecialOutputPackage
SpecialFunctionUnivariateTaylorSeries        SquareFreeRegularSetDecompositionPackage
StreamExponentialSeriesOperations      StreamExponentialSeriesTranscendentalFunctions
StreamInfiniteProduct      StreamTensor       STransformPackage      StreamFunctions1
StreamFunctions2             StreamFunctions3            StreamTaylorSeriesOperations
StreamTranscendentalFunctions             StreamTranscendentalFunctionsNonCommutative
SubResultantPackage                FunctionSpaceSum               RationalFunctionSum
SparseUnivariatePolynomialFunctions2     SupFractionFactorizer     SymmetricFunctions
TableauxBumpers           TabulatedComputationPackage           TensorPowerFunctions2
UnittestCount      TexFormat1      TopLevelThreeSpace      TriangularMatrixOperations
TubePlotTools     UserDefinedPartialOrdering     UnivariateFormalPowerSeriesFunctions
UnitGaussianElimination     UnivariateLaurentSeriesFunctions2     UnivariateFactorize
UniversalSegmentFunctions2                             UnivariatePolynomialFunctions2
UnivariatePolynomialCommonDenominator        UnivariatePolynomialDecompositionPackage
UnivariatePolynomialDivisionPackage         UnivariatePolynomialMultiplicationPackage
UnivariatePolynomialCategoryFunctions2                 UnivariatePolynomialSquareFree
UnivariatePuiseuxSeriesFunctions2                    UnivariateTaylorSeriesFunctions2
UnivariateTaylorSeriesODESolver     UTSodetools      TaylorSolve     VectorFunctions2
ViewportPackage               ViewDefaultsPackage              WeierstrassPreparation
WildFunctionFieldIntegralBasis              XExponentialPackage             ExportXml
ParadoxicalCombinatorsForStreams  ZeroDimensionalSolvePackage IntegerLinearDependence
AbelianMonoid      AlgebraicallyClosedField     Aggregate      AlgebraicFunctionField
AbelianMonoidRing  AnonymousFunction  Any  OneDimensionalArray  ArrayStack  BasicType
BagAggregate   BinaryFile   Boolean   BalancedPAdicInteger   BinaryRecursiveAggregate
BitAggregate BinaryTournament  CardinalNumber CharacterClass ComplexDoubleFloatVector
Character  CliffordAlgebra  Commutator   Complex  ContinuedFraction  DecimalExpansion
DeRhamComplex    DoubleFloatMatrix    DirectedGraph    Dictionary    DifferentialRing
DirectProductCategory    DirichletRing     DivisionRing    ElementaryFunctionCategory
EuclideanModularRing    Equation   ExpressionSpace    Evalable   ExponentialExpansion
FreeAbelianMonoid      FlexibleArray      FourierComponent      FiniteDivisorCategory
FortranExpression    FunctionFieldCategory   FreeModule    FreeMonoid   FunctionGraph
ScriptFormulaFormat      FortranProgram      FieldOfPrimeCharacteristic      Factored
FramedAlgebra    FractionalIdeal    FramedNonAssociativeAlgebra    FiniteSetAggregate
FortranScalarType  FortranTemplate  GcdDomain  Heap  HexadecimalExpansion  HTMLFormat
InnerAlgebraicNumber IndexedTwoDimensionalArray  IndexCard IndexedDirectProductObject
InnerFreeAbelianMonoid    InnerFiniteField   IndexedList    IndexedMatrix   InputForm
Integer   Interval  InnerPrimeField   IndexedString  InfiniteTuple   IndexedAggregate
JetBundleExpression   JetBundleLinearFunction  JetBundleSymAna   JetBundleXExpression
JetDifferential    AssociatedJordanAlgebra    KeyedAccessFile   Kernel    LeftAlgebra
LaurentPolynomial    Library     LieAlgebra    ListMultiDictionary    LinearAggregate
Loop   ListAggregate   LyndonWord  ThreeDimensionalMatrix   ModularAlgebraicGcdTools3
MatrixCategory     MachineComplex    MachineFloat     MakeCachableSet    ModularField
ModuleMonomial   ModularRing   MoebiusTransform   MonadWithUnit   Monoid   MonoidRing
OrderlyDifferentialVariable  OrderedFreeMonoid   OpenMathConnection  OpenMathEncoding
OpenMathErrorKind     Operator     OrderedRing     OrdSetInts     OrderedVariableList
PAdicInteger    PAdicRationalConstructor    ParametricPlaneCurve    ParametricSurface
PatternMatchResult  PartialDifferentialRing  Permutation  PrimeField  PositiveInteger
Plot3D   Polynomial  PartialOrder   PrimitiveArray  Partition   PolynomialSetCategory
QueryEquation    QuadraticForm     GeneralQuaternion    Queue    RadicalFunctionField
RecursiveAggregate    RealClosure     RegularTriangularSet    Result    RetractableTo
Ring       RectangularMatrix       RealNumberSystem       RecursivePolynomialCategory
RuleCalled   SimpleAlgebraicExtension   SArgand    SimpleCell   SceneIFS   SConformal
SegmentBinding    Sequence    SetAggregate    SetOfMIntegersInOneToN    SExpressionOf
SemiGroup   SKICombinators  SparseMultivariatePolynomial   VectorModularReconstructor
VectorCategory    TwoDimensionalViewport    Void   VectorSpace    WeightedPolynomials
XDistributedPolynomial   XHashTable   XmlElement   XPolynomial   XRecursivePolynomial
AlgFactor    AlgebraicMultFact    AnyFunctions1   AssociatedEquations    BezoutMatrix
BoundIntegerRoots    GosperSummationMethod    GraphicsDefaults   Guess    GuessExpBin
GuessFiniteFunctions GuessPolynomial  GuessPolynomialInteger HallBasis InnerAlgFactor
IntegralBasisPolynomialTools    IdealDecompositionPackage    InnerNumericEigenPackage
InputFormFunctions1   InnerMultFact  InfiniteProductPrimeField   AlgebraicIntegration
IntegerBits  ElementaryIntegration  GenusZeroIntegration  AlgebraicHermiteIntegration
PatternMatchIntegration          IntegerRetractions         TranscendentalIntegration
InternalPrintPackage       IntegrationResultToFunction       IrredPolyOverFiniteField
ModularHermitianRowReduction MakeFloatCompiledFunction MakeRecord MultivariateLifting
ModularHermitePade     MomentPackage    MPolyCatFunctions2     MPolyCatPolyFactorizer
MonoidRingFunctions2            MoreSystemCommands           MultiplicativeDependence
MultivariateSquareFree       NumericComplexEigenPackage      NumberFieldIntegralBasis
NonLinearSolvePackage   NoneFunctions1  NormalizationPackage   NPCoef  RadixUtilities
RationalFactorize       RDEaux      ElementaryRischDESystem       ElementaryRischDEX2
TranscendentalRischDESystem   ReducedDivisor    RealZeroPackageQ   RecurrenceOperator
RadicalEigenPackage   RepresentationPackage2   RepeatedSquaring   RetractSolvePackage
RandomFloatDistributions       RationalFunctionFactorizer       RationalInterpolation
SymmetryAnalysis  SystemSolvePackage TangentExpansions  TemplateUtilities UnittestAux
Unittest    ToolsForSign    TrigonometricManipulations    TranscendentalManipulations
TwoFactorize UserDefinedVariableOrdering
"""

spad_commands = command_list.split()



if __name__ == '__main__':
    from ipykernel.kernelapp import IPKernelApp
    path=os.path.dirname(os.path.abspath(__file__))
    req_asdf='(require :asdf)'
    req_ht='(require :hunchentoot)'
    ld_webspad='(load "{0}/webspad")'.format(path)
    start='(defvar webspad::fricas-acceptor (webspad::start {0} "localhost"))'.format(htport)
    ev1=')lisp (progn {0} {1} {2})'.format(req_asdf,req_ht,ld_webspad) 
    ev2=')lisp {0}'.format(start)
    fopts = fricas_start_options
    pid = Popen(fricas_terminal + ['fricas','-eval',ev1,'-eval',ev2,fopts])
    IPKernelApp.launch_instance(kernel_class=SPAD)


