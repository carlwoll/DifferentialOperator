(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Dec 25, 2017 *)

BeginPackage["DifferentialOperator`"]
(* Exported symbols added here with SymbolName::usage *) 

DifferentialOperator::usage = "DifferentialOperator[x__] is equivalent to D[#, xx]&, but it shouldn't need to be used"

Begin["`Private`"]
(* Implementation of the package *)

CurrentValue[EvaluationNotebook[], InputAutoReplacements] = {
	"pd" -> TemplateBox[
		{"\"\[PartialD]\""},
		"Partial",
		DisplayFunction -> (StyleBox[#, ShowStringCharacters->False]&),
		InterpretationFunction -> (
			RowBox[{"DifferentialOperator`Private`operator", "[", RowBox[{"DifferentialOperator","[","]"}],"]"}]&
		),
		Editable->False,
		Selectable->False
	],
	ParentList
};

DifferentialOperator[z__:Global`x][f_] := D[f, z]

DifferentialOperator /: DifferentialOperator[z__:Global`x]^n_Integer?Positive := Apply[
	DifferentialOperator,
	Flatten @ ConstantArray[{z}, n]
]

DifferentialOperator /: MakeBoxes[DifferentialOperator[], form_] := InterpretationBox[
	"\[PartialD]", DifferentialOperator[]
]

DifferentialOperator /: MakeBoxes[DifferentialOperator[x__], form_] := With[
	{sub = RowBox @ BoxForm`MakeInfixForm[{x},",", form]},

	InterpretationBox[
		SubscriptBox["\[PartialD]",sub],
		DifferentialOperator[x]
	]
]

(* CenterDot *)
	
(* arithmetic *)
CenterDot[___, 0, ___] = 0;

CenterDot[a_, c__] + CenterDot[b_, c__] ^:= CenterDot[a+b,c]
a_?scalarQ CenterDot[b_, c___] ^:= CenterDot[a b, c]

SetAttributes[CenterDot,{Flat,OneIdentity}]

(* nested function application *)
CenterDot[a__, b_][x_] := CenterDot[a][CenterDot[b][x]]

(* function application *)
CenterDot[a_Plus][x_] := CenterDot[#][x]&/@a
CenterDot[a_?scalarQ][x_] := a x
CenterDot[a_?scalarQ b_?differentialQ][x_] := a CenterDot[b][x]
CenterDot[d_DifferentialOperator][x_] := d[x]

(* operator wrapper *)
SetAttributes[operator, {Flat, OneIdentity}]

(* addition *)
operator[a_]+c_ ^:= operator[a+c]
	
(* scalar multiplication *)
c_?scalarQ operator[a_] ^:= operator[c a]
	
(* composition *)
operator[a_,b__] := operator[CenterDot[a,b]]
	
operator[a_]\[CenterDot]c_ ^:= operator[CenterDot[a,c]]
c_\[CenterDot]operator[a_] ^:= operator[CenterDot[c,a]]
	
(* power *)
operator /: operator[a_]^n_Integer := operator[CenterDot@@ConstantArray[a,n]]

(* subscripted nabla *)
Subscript[operator[DifferentialOperator[]], x__] ^:= operator[DifferentialOperator[x]]

(* utilities*)
scalarQ = FreeQ[DifferentialOperator];
differentialQ = Not @* scalarQ;

(* function application *)
operator[a_][x_] := CenterDot[a][x]

operator/:MakeBoxes[operator[a__], form_]:=If[Length@Hold[a]>1,
	StyleBox[MakeBoxes[CenterDot[a], form], Bold],
	StyleBox[MakeBoxes[a, form], Bold]
]

End[]

EndPackage[]

