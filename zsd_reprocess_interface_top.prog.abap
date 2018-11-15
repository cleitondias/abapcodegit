*&---------------------------------------------------------------------*
*&  Include  zsd_reprocess_interface_top
*&---------------------------------------------------------------------*

CLASS cl_handler_evt DEFINITION DEFERRED.

INCLUDE <icon>.

TABLES:
  vbak,
  likp.

CONSTANTS:
  c_ctt(500)  TYPE c VALUE 'http://www.ctt.pt/feapl_2/app/open/objectSearch/cttexpresso_feapl_132col-cttexpressoObjectSearch.jspx?showResults=true&pesqObjecto.objectoId=',
  c_chnr(500) TYPE c VALUE 'https://www.chronopost.pt/track-and-trace?reference='.

TYPES:
  BEGIN OF ty_outtab,
    status        TYPE char4,
    interface     TYPE zsd_log_interf-interface,
    vbeln_va      TYPE vbak-vbeln,
    vbeln_vl      TYPE likp-vbeln,
    erdat         TYPE likp-erdat,
    tracking_num  TYPE zsd_log_interf-tracking_num,
    idloja        TYPE zsd_b2c_pickp_t-idloja,
    lnk_bt        TYPE char4,
    message       TYPE zsd_log_interf-message,
    link          TYPE zsd_log_interf-link,
    prod_cancel   TYPE zsd_log_interf-prod_cancel,
    subproduct_id TYPE zsd_log_interf-subproduct_id,
  END OF ty_outtab,

  tt_outtab TYPE STANDARD TABLE OF ty_outtab WITH EMPTY KEY.

*----------------------------------------------------------------------*
* Internal Table
*----------------------------------------------------------------------*
DATA:
  t_xlikp    TYPE shp_likp_t,
  t_xlips    TYPE shp_lips_t,
  t_xvbpa    TYPE shp_vbpavb_t,
  t_xvbfa    TYPE shp_vl10_vbfa_t,
  st_xvbak   TYPE vbak,
  t_xvbap    TYPE oi0_vbapvb_tt,
  st_xtvakt  TYPE tvakt,
  lst_return TYPE zsd_log_interf,
  t_saida    TYPE TABLE OF ty_outtab,
  t_selected TYPE TABLE OF lvc_s_row,
  t_exclude  TYPE ui_functions,
  t_where    TYPE TABLE OF char100.

DATA:
    lst_where LIKE LINE OF t_where.

*----------------------------------------------------------------------*
* Variables
*----------------------------------------------------------------------*
DATA:
* Reference to document
  dg_dyndoc_id   TYPE REF TO cl_dd_document,
* Reference to split container
  dg_splitter    TYPE REF TO cl_gui_splitter_container,
* Reference to grid container
  dg_parent_grid TYPE REF TO cl_gui_container,
* Reference to html container
  dg_html_cntrl  TYPE REF TO cl_gui_html_viewer,
* Reference to html container
  dg_parent_html TYPE REF TO cl_gui_container,
  w_container    TYPE REF TO cl_gui_custom_container,
  w_alv          TYPE REF TO cl_gui_alv_grid,
  w_handler_evt  TYPE REF TO cl_handler_evt,
  w_interfaces   TYPE REF TO zcl_processa_interfaces_transp.
