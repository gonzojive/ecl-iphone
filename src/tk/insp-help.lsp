;******************************************************************************
;
; Project       : STk-inspect, a graphical debugger for STk
;
; File name     : inspect-help.stk
; Creation date : Sep-16-1993
; Last update   : Sep-17-1993
;
;******************************************************************************
;
; This file contains help variables.
;
;******************************************************************************

(in-package "TK")

(provide "inspect-help")

(define STk-inspect-help '("STF-0.1" "
STk-inspect\n\n
STk-inspect is a graphical inspector for STk (the Scheme/Tk based language).

It is the firt step toward a more general debugger . For now, it only permits to visualize and modify STk objects. 

Original code written by Eric Fintzel (fintzel@kaolin.unice.fr)
Hacked for version 2 by Erick Gallesio (eg@unice.fr)
" ((bold-italic-12 ("5.0" "5.11" "5.41" "5.44")) (roman-18 ("2.0" "2.11")) (fixed ("9.39" "9.62" "10.40" "10.51")) (normal ("2.11" "5.0")))))

;---- Help for (inspect,help) "General inspector"

(define General-Inspector-help '("STF-0.1" "
General inspector


The General inspector is the simplest tool to trace STk objects. It allows you to watch and modify every kind of object.

To inspect an object

You can use the STk 

        (inspect object) 

command. For instance, if you want to inspect the variable my-variable, just type:
          STk> (inspect 'my-variable)

You can also select the Inspect option in an other STk-inspect tool (detailer, viewer,...).

To uninspect an object

You can use the STk

        (uninspect object) 

command, or you can select the Uninspect option in the Icon menu associated with the object line inspector.

Screen organization                   

The General inspector is a toplevel window divided in 2 areas: a menu bar and a list of all inspected objects.

Menu area
Menu area comports two buttons:

         Command menu
         The Uninspect all option uninspects all inspected objects and destroy the           General inspector window.
         The Undebug option destroys all the STk-inspect toplevel windows.

         Help menu
         The STk-inspect option gives a general help about STk-inspect.
         The General inspector option gives this help.

Inspected objects area
Each line of the list concerns one object, a line is divided in 3 fields:

        The Object field contains the inspected object.

        The Value field contains the value of the object (the result of the object evaluation). You can modify the object by editing this field. A <Return> validation affects to the object the result of the field contents evaluation, while a <Shift-Return> validation  affects to the object the field contents without evaluation.

        The Icon provides actions on the object.
" ((bold-italic-12 ("7.0" "7.20" "18.0" "18.22" "26.0" "26.19" "38.13" "38.24")) (italic-12 ("16.69" "16.89" "24.31" "24.40" "28.65" "28.73" "28.80" "28.84" "33.9" "34.0" "34.13" "34.26" "35.13" "35.20" "37.9" "38.0" "39.13" "39.30" "44.12" "44.19" "46.12" "46.18" "48.12" "48.16")) (roman-18 ("2.0" "2.17")) (roman-12 ("7.20" "8.0" "30.0" "30.9" "41.0" "41.22")) (fixed ("11.8" "11.24" "13.59" "13.70" "14.10" "14.37" "22.8" "22.26" "46.147" "46.155" "46.242" "46.257")) (underline ("2.0" "3.0" "7.0" "7.20" "18.0" "18.22" "26.0" "26.19" "33.9" "34.0" "37.9" "38.0")))))


;---- Help for (detail,help) "Detailer"

(define Detailer-help '("STF-0.1" "
Detailer

A detailer is a STk-inspect tool which allows you to see STk objects with more details.

For instance, a list detailer shows you the structure of a list where you can edit each element of the list; a procedure detailer permits you to edit the body of a procedure; a widget detailer allows you to access directly the whole option fields of a widget; and so on...

For now, there are detailers for lists, pairs vectors, procedures and widgets.

To detail an object

You can use the STk 

        (detail object)

command. For example, if you want to detail the list my-list, type:

        STk> (detail my-list)

You can also select the Detail option in an other STk-inspect tool (inspector, viewer, ...).

To undetail an object

You can use the STk 

        (undetail object)

command or you can select the Undetail option in the Command menu of the object detailer window.
" ((bold-italic-12 ("10.0" "10.19" "22.0" "22.21")) (italic-12 ("6.16" "6.29" "6.111" "6.130" "6.177" "6.192" "8.33" "8.77" "16.53" "16.60" "20.24" "20.30" "20.68" "20.77" "20.79" "20.90" "22.21" "23.0" "28.30" "28.38" "28.53" "28.65")) (roman-18 ("2.0" "2.8")) (fixed ("14.8" "14.23" "18.8" "19.0" "26.8" "26.25")) (underline ("2.0" "2.8" "10.0" "10.19" "22.0" "23.0")))))

;---- Help for (detail,help) "List detailer"

(define List-detailer-help '("STF-0.1" "
List detailer


A list detailer is a STk-inspect tool which shows you the structure of a list.

The list detailer window is divided in 3 areas:
the Id area, a menu bar and the elements list.

Id area

This area identifies the list you detail, it contains 2 fields:

        The object field contains the object detailed.

        The Value field contains the value of the object (a list resulting of the object evaluation). You can modify the object by editing this field. A  <Return> validation affects to the object the result of the field contents evaluation, while a <Shift-Return> validation affects to the object the field contents without evaluation.

After a modification, if the object type is no more a list, the detailer is adapted.


Menu bar

Two sub-menus are provided:

        Command menu
        The Inspect  option calls the General inspector for the detailed object.
        The Undetail option destroys the list detailer window.
        The View option calls the Viewer for the detailed object.

        Help menu
        The STk-inspect option gives a general help about STk-inspect.
        The Detailer option gives a general help about detailers tools.
        The List detailer option gives this help.

Elements list

This area displays all the elements of the detailed list (with indexes). You can select a particular element with mouse button 1, its value will appear on the Value <index> entry, and you can modify it (validation of your entry can be done using the current way: <Return> and <Shift-Return>)).
" ((tty-12 ("16.154" "16.162" "16.249" "16.264" "37.160" "37.174" "37.264" "37.272" "37.277" "37.292")) (bold-italic-12 ("10.0" "10.7" "21.0" "22.0" "35.0" "35.13")) (italic-12 ("8.4" "8.11" "8.15" "8.24" "8.32" "8.45" "14.12" "14.18" "16.12" "16.17" "25.8" "25.20" "26.12" "26.21" "27.12" "27.20" "28.12" "28.16" "30.8" "31.0" "31.12" "31.23" "32.12" "32.20" "33.12" "33.25")) (roman-18 ("2.0" "2.13")) (underline ("2.0" "2.13" "10.0" "10.7" "21.0" "22.0" "25.8" "25.20" "30.8" "31.0" "35.0" "35.13")))))



;---- Help for (detail,help) "Pair detailer"

(define Pair-detailer-help '("STF-0.1" "
Pair detailer


A pair detailer is a STk-inspect tool which shows you the structure of a pair.

The pair detailer window is divided in 3 areas:
the Id area, a menu bar and the elements list.

Id area

This area identifies the pair you detail, it contains 2 fields:

        The object field contains the object detailed.

        The Value field contains the value of the object (a pair resulting of the object evaluation). You can modify the object by editing this field. A  <Return> validation affects to the object the result of the field contents evaluation, while a <Shift-Return> validation affects to the object the field contents without evaluation.

After a modification, if the object type is no more a pair, the detailer is adapted.


Menu bar

Two sub-menus are provided:

        Command menu
        The Inspect  option calls the General inspector for the detailed object.
        The Undetail option destroys the pair detailer window.
        The View option calls the Viewer for the detailed object.

        Help menu
        The STk-inspect option gives a general help about STk-inspect.
        The Detailer option gives a general help about detailers tools.
        The List detailer option gives this help.

Elements list

This area displays all the elements of the detailed list (with indexes). Note that the last element has a pointed index. You can select a particular element with mouse button 1, its value will appear on the Value <index> entry, and you can modify it (validation of your entry can be done using the current way: <Return> and <Shift-Return>)).
" ((tty-12 ("16.154" "16.162" "16.249" "16.264" "37.207" "37.221" "37.311" "37.319" "37.324" "37.339")) (bold-italic-12 ("10.0" "10.7" "21.0" "22.0" "35.0" "35.13")) (italic-12 ("8.4" "8.11" "8.15" "8.24" "8.32" "8.45" "14.12" "14.18" "16.12" "16.17" "25.8" "25.20" "26.12" "26.21" "27.12" "27.20" "28.12" "28.16" "30.8" "31.0" "31.12" "31.23" "32.12" "32.20" "33.12" "33.25")) (roman-18 ("2.0" "2.13")) (underline ("2.0" "2.13" "10.0" "10.7" "21.0" "22.0" "25.8" "25.20" "30.8" "31.0" "35.0" "35.13"))))
)


;---- Help for (detail,help) "Vector detailer"

(define Vector-detailer-help '("STF-0.1" "
Vector detailer


A vector detailer is a STk-inspect tool which shows you the structure of a vector.

The vector detailer window is divided in 3 areas:
the Id area, a menu bar and the elements list.

Id area

This area identifies the vector you detail, it contains 2 fields:

        The object field contains the object detailed.

        The Value field contains the value of the object (a vector resulting of the object evaluation). You can modify the object by editing this field. A  <Return> validation affects to the object the result of the field contents evaluation, while a <Shift-Return> validation affects to the object the field contents without evaluation.

After a modification, if the object type is no more a vector, the detailer is adapted.


Menu bar

Two sub-menus are provided:

        Command menu
        The Inspect  option calls the General inspector for the detailed object.
        The Undetail option destroys the vector detailer window.
        The View option calls the Viewer for the detailed object.

        Help menu
        The STk-inspect option gives a general help about STk-inspect.
        The Detailer option gives a general help about detailers tools.
        The List detailer option gives this help.

Elements list

This area displays all the elements of the detailed vector (with indexes). Note that the last element has a pointed index. You can select a particular element with mouse button 1, its value will appear on the Value <index> entry, and you can modify it (validation of your entry can be done using the current way: <Return> and <Shift-Return>)).
" ((tty-12 ("16.156" "16.164" "16.251" "16.266" "37.209" "37.223" "37.313" "37.321" "37.326" "37.341")) (bold-italic-12 ("10.0" "10.7" "21.0" "22.0" "35.0" "35.13")) (italic-12 ("8.4" "8.11" "8.15" "8.24" "8.32" "8.45" "14.12" "14.18" "16.12" "16.17" "25.8" "25.20" "26.12" "26.21" "27.12" "27.20" "28.12" "28.16" "30.8" "31.0" "31.12" "31.23" "32.12" "32.20" "33.12" "33.25")) (roman-18 ("2.0" "3.0")) (underline ("2.0" "3.0" "10.0" "10.7" "21.0" "22.0" "25.8" "25.20" "30.8" "31.0" "35.0" "35.13"))))
)


;---- Help for (detail,help) "Procedure detailer"

(define Procedure-detailer-help '("STF-0.1" "
Procedure detailer


A procedure detailer is a STk-inspect tool which allows you to see and edit the body of a procedure.

The procedure detailer window is divided in 3 areas:
the Id area, a menu bar and the body area.

Id area

This area identifies the procedure you detail, it contains 2 fields:

        The object field contains the object detailed.

        The Value field contains the value of the object (a procedure resulting of the object evaluation). You can modify the object by editing this field. A  <Return> validation affects to the object the result of the field contents evaluation, while a <Shift-Return> validation affects to the object the field contents without evaluation.

After a modification, if the object type is no more a procedure, the detailer is adapted.


Menu bar

Two sub-menus are provided:

        Command menu
        The Inspect  option calls the General inspector for the detailed object.
        The Undetail option destroys the procedure detailer window.
        The View option calls the Viewer for the detailed object.

        Help menu
        The STk-inspect option gives a general help about STk-inspect.
        The Detailer option gives a general help about detailers tools.
        The List detailer option gives this help.

Body area

This area displays the procedure body, and allows you to edit it." ((tty-12 ("16.156" "16.164" "16.251" "16.266")) (bold-italic-12 ("10.0" "10.7" "21.0" "22.0" "35.0" "37.0")) (italic-12 ("8.4" "8.11" "8.15" "8.24" "8.32" "8.41" "14.12" "14.18" "16.12" "16.17" "25.8" "25.20" "26.12" "26.21" "27.12" "27.20" "28.12" "28.16" "30.8" "31.0" "31.12" "31.23" "32.12" "32.20" "33.12" "33.25")) (roman-18 ("2.0" "3.0")) (normal ("37.0" "37.65")) (underline ("2.0" "3.0" "10.0" "10.7" "21.0" "22.0" "25.8" "25.20" "30.8" "31.0" "35.0" "37.0"))))
)


;---- Help for (detail,help) "Widget detailer"

(define Widget-detailer-help '("STF-0.1" "
Widget detailer


A widget detailer is a STk-inspect tool which allows you to see and edit the characteristics of a widget.

The widget detailer window is divided in 3 areas:
the Id area, a menu bar and the option area.

Id area

This area identifies the widget you detail, it contains 2 fields:

        The object field contains the object detailed.

        The Value field contains the widget name of the object.

After a modification, if the object type is no more a widget, the detailer is adapted.


Menu bar

Three sub-menus are provided:

        Command menu
        The Inspect  option calls the General inspector for the detailed object.
        The Undetail option destroys the widget detailer window.
        The View option calls the Viewer for the detailed object.

         Bindings menu
        This menu contains all the bindings associated with the widget (the widget bindings are listed before the widget class bindings). You can select a binding to edit it.

        Help menu
        The STk-inspect option gives a general help about STk-inspect.
        The Detailer option gives a general help about detailers tools.
        The Widget detailer option gives this help.

Option area

This area displays all the options of the detailed widget. You can edit an option using the current way <Return> and <Shift-Return>" ((tty-12 ("40.104" "40.113" "40.117" "40.131")) (bold-italic-12 ("10.0" "10.7" "21.0" "22.0" "38.0" "40.0")) (italic-12 ("8.4" "8.11" "8.15" "8.24" "8.32" "8.43" "14.12" "14.18" "16.12" "16.17" "25.8" "25.20" "26.12" "26.21" "27.12" "27.20" "28.12" "28.16" "30.8" "31.0" "33.8" "34.0" "34.12" "34.23" "35.12" "35.20" "36.12" "36.27")) (roman-18 ("2.0" "3.0")) (normal ("31.0" "33.0" "40.0" "40.104" "40.113" "40.117")) (underline ("2.0" "3.0" "10.0" "10.7" "21.0" "22.0" "25.8" "25.20" "30.8" "31.0" "33.8" "34.0" "38.0" "40.0"))))
)


;---- Help for (view,help) "Viewer"

(define Viewer-help '("STF-0.1" "
Viewer

A viewer is a STk-inspect tool which gives you a graphical representation of STk objects.

All object types can be viewed; particular viewer are provided for procedures and widgets.

To view an object

You can use the STk 

         (view object) 

command. For example, if you want to view the list my-list, type:

        (view 'my-list)

You can also select the View option in an other STk-inspect tool.

To unview an object

You can use the STk 

        (unview object) 

command. Or you can select the Unview option in the Command menu of the object viewer window." ((tty-12 ("12.9" "13.0" "14.51" "14.59" "16.8" "17.0" "24.8" "24.24")) (bold-italic-12 ("8.0" "8.17" "20.0" "21.0")) (italic-12 ("6.67" "6.77" "6.82" "6.89" "18.24" "18.28" "26.31" "26.37" "26.52" "26.59")) (roman-18 ("2.0" "2.6")) (underline ("2.0" "2.6" "8.0" "8.17" "20.0" "21.0"))))
)
