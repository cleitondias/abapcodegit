*&---------------------------------------------------------------------*
*&  Include  zsd_reprocess_interface_form
*&---------------------------------------------------------------------*

FORM f_select_data TABLES pt_saida.

  TYPES:
    BEGIN OF ly_table,
      vbeln TYPE zsd_log_interf-vbeln_va,
      erdat TYPE likp-erdat,
    END OF ly_table.

  DATA:
    lt_table  TYPE TABLE OF ly_table,
    lw_table  TYPE char20,
    r_status  TYPE RANGE OF zsd_log_interf-status,
    lw_status LIKE LINE OF r_status.

  FIELD-SYMBOLS:
  <table> TYPE ANY TABLE.


  DATA(lw_interface) = COND string(  WHEN p_ctt  IS NOT INITIAL THEN |002|
                                     WHEN p_chnr IS NOT INITIAL THEN |003|
                                     WHEN p_tnb  IS NOT INITIAL THEN |001| ).

  lw_status-sign   = |I|.
  lw_status-option = |EQ|.
  IF p_send IS NOT INITIAL.
    lw_status-low = |1|.
    APPEND lw_status TO r_status.
  ENDIF.

  IF p_nsend IS NOT INITIAL.
    lw_status-low = |4|.
    APPEND lw_status TO r_status.
  ENDIF.

  IF p_errors IS NOT INITIAL.
    lw_status-low = |2|.
    APPEND lw_status TO r_status.
    lw_status-low = |3|.
    APPEND lw_status TO r_status.
  ENDIF.

  lst_where = |INTERFACE EQ @LW_INTERFACE|.
  APPEND lst_where TO t_where.

  IF p_ctt IS NOT INITIAL OR p_chnr IS NOT INITIAL.
    lst_where = |AND VBELN_VL IN @S_DELIV|.
    APPEND lst_where TO t_where.
    lst_where = |AND VBELN_VA IN @S_SALES|.
  ELSE.
    lst_where = |AND VBELN_VA IN @S_SALES|.
  ENDIF.
  APPEND lst_where TO t_where.

  lst_where = |AND STATUS IN @R_STATUS|.
  APPEND lst_where TO t_where.

  SELECT *
   FROM zsd_log_interf
   INTO TABLE @DATA(t_log_interf)
   WHERE (t_where).

  IF sy-subrc IS INITIAL.

    FREE t_where.
    IF p_ctt IS NOT INITIAL.
      lst_where = |VBELN EQ T_LOG_INTERF-VBELN_VL|.
      lw_table  = |LIKP|.
    ELSE.
      lst_where = |VBELN EQ T_LOG_INTERF-VBELN_VA|.
      lw_table  = |VBAK|.
    ENDIF.
    APPEND lst_where TO t_where.

    IF s_erdat[] IS NOT INITIAL.
      lst_where = |AND ERDAT IN S_ERDAT|.
      APPEND lst_where TO t_where.
    ENDIF.

    SELECT vbeln erdat
     FROM (lw_table)
     INTO TABLE lt_table
     FOR ALL ENTRIES IN t_log_interf
     WHERE (t_where).

    SELECT *
     FROM vbkd
     INTO TABLE @DATA(t_vbkd)
     FOR ALL ENTRIES IN @t_log_interf
     WHERE vbeln EQ @t_log_interf-vbeln_va.
    IF sy-subrc IS INITIAL.
      SORT t_vbkd BY vbeln bstkd_e.
      DELETE ADJACENT DUPLICATES FROM t_vbkd COMPARING vbeln bstkd_e.
    ENDIF.

    SORT t_log_interf BY vbeln_va vbeln_vl.


    DATA(t_outtab) = VALUE tt_outtab( FOR lst_log_interf IN t_log_interf
                                      FOR lst_table IN lt_table WHERE ( vbeln EQ lst_log_interf-vbeln_vl OR vbeln EQ lst_log_interf-vbeln_va )
                                      FOR lst_vbkd  IN t_vbkd   WHERE (  vbeln EQ lst_log_interf-vbeln_va )
                                       ( status = SWITCH #( lst_log_interf-status
                                                             WHEN '1' THEN icon_green_light
                                                             WHEN '2' THEN icon_red_light
                                                             WHEN '3' THEN icon_yellow_light
                                                             WHEN '4' THEN icon_dummy
                                                             WHEN '5' THEN icon_delivery_complete )
                                         interface = lst_log_interf-interface
                                         vbeln_va = lst_log_interf-vbeln_va
                                         vbeln_vl = lst_log_interf-vbeln_vl
                                         erdat    = lst_table-erdat
                                         tracking_num = lst_log_interf-tracking_num
                                         idloja       = lst_vbkd-bstkd_e
                                         lnk_bt   = COND string(  WHEN lst_log_interf-link IS NOT INITIAL THEN icon_link
                                                                  WHEN lst_log_interf-link IS INITIAL THEN icon_wf_workitem_error )
                                         message  = lst_log_interf-message
                                         link     = lst_log_interf-link
                                         prod_cancel = lst_log_interf-prod_cancel
                                         subproduct_id = lst_log_interf-subproduct_id ) ).
    IF t_outtab[] IS NOT INITIAL.
      APPEND LINES OF t_outtab TO pt_saida.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_display.

  IF t_saida[] IS NOT INITIAL.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE text-m01 TYPE 'S'  DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM f_alv.

  DATA:
    lst_layout  TYPE lvc_s_layo,
    lst_variant TYPE disvariant,
    t_fcat      TYPE lvc_t_fcat.

  IF w_container IS INITIAL.

    IF w_alv IS BOUND.
      w_alv->free( ).
    ENDIF.

    lst_layout-cwidth_opt = 'X'.
    lst_layout-zebra = 'X'.
    lst_variant-report = sy-repid.

    CREATE OBJECT w_container
      EXPORTING
        container_name = 'CONTAINER'.

* Create TOP-Document
    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

* Create Splitter for custom_container
    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = w_container
        rows    = 2
        columns = 1.
* Split the custom_container to two containers and move the reference
* to receiving containers g_parent_html and g_parent_grid
    "i am allocating the space for grid and top of page
    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_grid.

    "you can set the height of it
* Set height for g_parent_html
    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 10.

    CREATE OBJECT w_alv
      EXPORTING
        i_parent = dg_parent_grid.

***    CALL METHOD w_alv->register_edit_event
***      EXPORTING
***        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD w_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* setting focus for created grid control
    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = w_alv.

    CREATE OBJECT w_handler_evt.
    SET HANDLER w_handler_evt->handle_toolbar      FOR w_alv.
    SET HANDLER w_handler_evt->handle_hotspot      FOR w_alv.
    SET HANDLER w_handler_evt->handle_user_command FOR w_alv.
    SET HANDLER w_handler_evt->handle_button_click FOR w_alv.
    SET HANDLER w_handler_evt->handle_double_click FOR w_alv.
    SET HANDLER w_handler_evt->handle_top_of_page  FOR w_alv.


    PERFORM: f_built_fcat TABLES t_fcat,
             f_exclude.

    CALL METHOD w_alv->set_table_for_first_display
      EXPORTING
        i_save               = 'A'
*       i_default            = 'X'
        is_layout            = lst_layout
        is_variant           = lst_variant
        it_toolbar_excluding = t_exclude
      CHANGING
        it_outtab            = t_saida
        it_fieldcatalog      = t_fcat.

    CALL METHOD dg_dyndoc_id->initialize_document.

* Processing events
    CALL METHOD w_alv->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = dg_dyndoc_id.

* set editable cells to ready for input
    CALL METHOD w_alv->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD w_alv->set_toolbar_interactive.

  ELSE.
    CALL METHOD w_alv->refresh_table_display.
    CALL METHOD cl_gui_cfw=>flush.
  ENDIF.

ENDFORM.

FORM f_built_fcat TABLES pt_fcat STRUCTURE lvc_s_fcat.

  DATA:
          lst_fcat TYPE lvc_s_fcat.

  DEFINE add_fcat.
    clear lst_fcat.
    lst_fcat-col_pos   = &1.
    lst_fcat-fieldname = &2.
    lst_fcat-coltext   = &3.
    lst_fcat-outputlen = &4.
    lst_fcat-hotspot   = &5.
    lst_fcat-icon      = &6.
    lst_fcat-style     = &7.
    lst_fcat-no_out    = &8.
    append lst_fcat to pt_fcat.
  END-OF-DEFINITION.

  DATA(lw_chrono) = COND string( WHEN p_tnb  IS NOT INITIAL THEN |X|
                                 WHEN p_ctt  IS NOT INITIAL THEN |X|
                                 WHEN p_chnr IS NOT INITIAL THEN space ).

  DATA(lw_tnb) = COND string( WHEN p_tnb  IS NOT INITIAL THEN |X|
                              WHEN p_ctt  IS NOT INITIAL THEN space
                              WHEN p_chnr IS NOT INITIAL THEN space ).

  DATA(lw_cac) = COND string( WHEN p_tnb  IS NOT INITIAL THEN space
                              WHEN p_ctt  IS NOT INITIAL THEN |X|
                              WHEN p_chnr IS NOT INITIAL THEN |X| ).

  add_fcat:
  1 'STATUS'       'Status'           '4'   abap_false abap_true  space space,
  2 'VBELN_VA'     'Encomenda'        '10'  abap_true  abap_false space space,
  3 'VBELN_VL'     'Entrega'          '10'  abap_true  abap_false space space,
  4 'ERDAT'        'Dt. Documento'    '10'  abap_false abap_false space space,
  5 'TRACKING_NUM' 'Rastreamento'     '20'  abap_false abap_false space space,
  6 'IDLOJA'       'Local de Entrega' '10'  abap_false abap_false space lw_chrono ,
  7 'LNK_BT'       'Etiquetas'        '8'   abap_false abap_false cl_gui_alv_grid=>mc_style_button lw_tnb,
  8 'MESSAGE'      'Mensagem'         '100' abap_false abap_false space space,
  9 'PROD_CANCEL'  'Cancelado'        '20' abap_false abap_false space lw_cac.

ENDFORM.

FORM f_selected_row USING pw_erro.

  FREE: t_selected.

  CALL METHOD w_alv->get_selected_rows
    IMPORTING
      et_index_rows = t_selected.

  IF t_selected[] IS INITIAL.
    pw_erro = abap_true.
  ENDIF.

ENDFORM.

FORM f_reprocess_interface.

  DATA:
          lw_error TYPE char1.

  PERFORM f_selected_row USING lw_error.
  IF lw_error IS INITIAL.

    DATA(lw_interface) = COND string( WHEN p_ctt  IS NOT INITIAL THEN |002|
                                      WHEN p_chnr IS NOT INITIAL THEN |003|
                                      WHEN p_tnb  IS NOT INITIAL THEN |001| ).

    LOOP AT t_selected ASSIGNING FIELD-SYMBOL(<fs_selected>).

      READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_selected>-index.
      IF sy-subrc IS INITIAL.
        DATA(lst_saida) = <fs_saida>.

        SELECT SINGLE COUNT( * )
          FROM zsd_log_interf
          WHERE interface EQ lw_interface
            AND vbeln_va  EQ lst_saida-vbeln_va
            AND status    EQ '1'.
        IF sy-subrc IS NOT INITIAL.
          CASE lst_saida-interface.
            WHEN '001'. "Interface TNB
              PERFORM f_retrieve_sales_order USING lst_saida.
              PERFORM f_tnb CHANGING lst_saida.
            WHEN '002' OR '003'. "Interface CTT or Chronopost

              PERFORM f_retrieve_delivery USING lst_saida.

              IF lst_saida-interface EQ '002'.
                PERFORM f_ctt CHANGING lst_saida.
              ELSE.
                PERFORM f_chronopost CHANGING lst_saida.
              ENDIF.
          ENDCASE.
          <fs_saida> = lst_saida.
        ELSE.
          MESSAGE text-m03 TYPE 'S' DISPLAY LIKE 'E'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    MESSAGE text-m02 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

FORM f_ctt CHANGING pst_saida TYPE ty_outtab.

  DATA:
          lw_label TYPE zsd_label_data-label.

  DATA(lw_rc) = w_interfaces->create_shipment_ctt( EXPORTING
                                                     it_xlikp = t_xlikp
                                                     it_xlips = t_xlips
                                                     it_xvbpa = t_xvbpa
                                                     it_xvbfa = t_xvbfa
                                                   IMPORTING
                                                     es_return = lst_return ).
  IF lw_rc IS INITIAL.
    pst_saida-status = COND string( WHEN lst_return-status EQ |1| THEN icon_green_light ).
    pst_saida-lnk_bt = COND string( WHEN lst_return-link IS NOT INITIAL THEN icon_link
                                    WHEN lst_return-link IS INITIAL THEN icon_wf_workitem_error ).
    pst_saida-tracking_num = lst_return-tracking_num.
    pst_saida-message = lst_return-message.
    pst_saida-subproduct_id = lst_return-subproduct_id.
    lw_label = lst_return-link.
***    PERFORM f_print_ctt_etiquetas USING lw_label.
  ELSE.
    pst_saida-status = COND string( WHEN lst_return-status EQ |2| THEN icon_red_light
                                    WHEN lst_return-status EQ |3| THEN icon_yellow_light ).
    pst_saida-message = lst_return-message.
  ENDIF.


ENDFORM.

FORM f_exclude.

  DATA: lst_exclude TYPE ui_func.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

  lst_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND lst_exclude TO t_exclude.
  CLEAR lst_exclude.

ENDFORM.                    " F_EXCLUDE

FORM f_retrieve_delivery CHANGING pst_saida TYPE ty_outtab.

  FREE:
  t_xlikp,
  t_xlips,
  t_xvbpa,
  t_xvbfa.

  SELECT *
   FROM likp
*   INTO TABLE @DATA(t_likp)
   APPENDING CORRESPONDING FIELDS OF TABLE t_xlikp
   WHERE vbeln EQ pst_saida-vbeln_vl.

  IF sy-subrc IS INITIAL.

    SORT t_xlikp BY vbeln.

    SELECT *
     FROM lips
*     INTO TABLE @DATA(t_lips)
     APPENDING CORRESPONDING FIELDS OF TABLE t_xlips
     FOR ALL ENTRIES IN t_xlikp
     WHERE vbeln EQ t_xlikp-vbeln.

    IF sy-subrc IS INITIAL.
      SORT t_xlips BY vbeln posnr.

      SELECT *
        FROM vbpa
*        INTO TABLE @DATA(t_vbpa)
        APPENDING CORRESPONDING FIELDS OF TABLE t_xvbpa
        FOR ALL ENTRIES IN t_xlips
        WHERE vbeln EQ t_xlips-vbeln.

      IF sy-subrc IS INITIAL.
        SORT t_xvbpa BY vbeln posnr.
      ENDIF.

    ENDIF.

    SELECT *
      FROM vbfa
*      INTO TABLE @DATA(t_vbfa)
      APPENDING CORRESPONDING FIELDS OF TABLE t_xvbfa
      WHERE vbelv EQ pst_saida-vbeln_va
        AND vbeln EQ pst_saida-vbeln_vl.

    IF sy-subrc IS INITIAL.
      SORT t_xvbfa BY vbelv posnv vbeln posnn.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_chronopost CHANGING pst_saida TYPE ty_outtab.

  DATA(lw_rc) = w_interfaces->create_shipment_chronopost( EXPORTING
                                                             it_xlikp = t_xlikp
                                                             it_xlips = t_xlips
                                                             it_xvbpa = t_xvbpa
                                                             it_xvbfa = t_xvbfa
                                                           IMPORTING
                                                              es_return = lst_return ).

  IF lw_rc IS INITIAL.
    pst_saida-status = COND string( WHEN lst_return-status EQ |1| THEN icon_green_light ).
    pst_saida-lnk_bt = COND string( WHEN lst_return-link IS NOT INITIAL THEN icon_link
                                    WHEN lst_return-link IS INITIAL THEN icon_wf_workitem_error ).
    pst_saida-tracking_num = lst_return-tracking_num.
    pst_saida-link         = lst_return-link.
    pst_saida-message = lst_return-message.
  ELSE.
    pst_saida-status = COND string( WHEN lst_return-status EQ |2| THEN icon_yellow_light
                                    WHEN lst_return-status EQ |3| THEN icon_red_light ).
    pst_saida-message = lst_return-message.
  ENDIF.

ENDFORM.

FORM f_retrieve_sales_order USING pst_saida TYPE ty_outtab.

  SELECT SINGLE *
    FROM vbak
    INTO st_xvbak
    WHERE vbeln EQ pst_saida-vbeln_va
      AND vdatu IN s_vdatu.

  IF sy-subrc IS INITIAL.

    SELECT *
     FROM vbap
     INTO CORRESPONDING FIELDS OF TABLE t_xvbap
     WHERE vbeln EQ st_xvbak-vbeln.

    IF sy-subrc IS INITIAL.
      SORT t_xvbap BY vbeln posnr.
    ENDIF.

    SELECT *
      FROM vbpa
      INTO CORRESPONDING FIELDS OF TABLE t_xvbpa
      WHERE vbeln EQ st_xvbak-vbeln.
    IF sy-subrc IS INITIAL.
      SORT t_xvbpa BY vbeln.
    ENDIF.

    SELECT SINGLE *
     FROM tvakt
     INTO st_xtvakt
     WHERE spras EQ sy-langu
       AND auart EQ st_xvbak-auart.


  ENDIF.

ENDFORM.

FORM f_tnb CHANGING pst_saida TYPE ty_outtab.

  DATA(lw_rc) = w_interfaces->create_shipment_tnb( EXPORTING
                                                     vbak  = st_xvbak
                                                     tvakt = st_xtvakt
                                                     xvbap = t_xvbap
                                                     xvbpa = t_xvbpa
                                                   IMPORTING
                                                     es_return = lst_return ).

  IF lw_rc IS INITIAL.
    pst_saida-status = COND string( WHEN lst_return-status EQ |2| THEN icon_red_light
                                    WHEN lst_return-status EQ |1| THEN icon_green_light ).
    pst_saida-lnk_bt = COND string( WHEN lst_return-link IS NOT INITIAL THEN icon_link
                                    WHEN lst_return-link IS INITIAL THEN icon_wf_workitem_error ).
    pst_saida-tracking_num = lst_return-tracking_num.
    pst_saida-message = lst_return-message.
  ELSE.
    pst_saida-status = COND string( WHEN lst_return-status EQ |2| THEN icon_yellow_light
                                    WHEN lst_return-status EQ |3| THEN icon_red_light ).
    pst_saida-message = lst_return-message.
  ENDIF.

ENDFORM.

FORM f_popup_loja USING pst_saida TYPE ty_outtab.

  DATA:
    lo_alv_popup TYPE REF TO cl_salv_table,
    lw_functions TYPE REF TO cl_salv_functions_list,
    t_pickup     TYPE TABLE OF zsd_alv_list_interface.

  SELECT idloja nomedaloja moradadaloja lote
         codigopostal localidade pais
   FROM zsd_b2c_pickp_t
   INTO TABLE t_pickup
   WHERE idloja EQ pst_saida-idloja.

  IF sy-subrc IS INITIAL.

    SORT t_pickup BY idloja.
    DELETE ADJACENT DUPLICATES FROM t_pickup COMPARING idloja.

    TRY.

        cl_salv_table=>factory(
             IMPORTING
                r_salv_table = lo_alv_popup
             CHANGING
                t_table      = t_pickup
          ).

        lw_functions = lo_alv_popup->get_functions( ).
        lw_functions->set_all( 'X' ).

        IF lo_alv_popup IS BOUND.
          lo_alv_popup->set_screen_popup(
            start_column = 10
            end_column   = 100
            start_line   = 1
            end_line     = 10 ).

          lo_alv_popup->display( ).
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.

  ENDIF.

ENDFORM.

FORM f_print_ctt_etiquetas USING pw_label TYPE zsd_label_data-label.

  TYPES: tt_etiquetas TYPE STANDARD TABLE OF zsd_etiquetas_ctt WITH EMPTY KEY.

  DATA:
    lst_output_opt TYPE ssfcompop,
    lw_formname    TYPE rs38l_fnam,
    lst_param      TYPE ssfctrlop,
    t_label        TYPE TABLE OF zsd_label_data-label,
    t_etiqueta_ctt TYPE zsd_etiquetas_ctt_tt.

  SPLIT pw_label AT cl_abap_char_utilities=>newline INTO TABLE t_label.

  DATA(t_etiquetas) = VALUE tt_etiquetas( FOR lst_label IN t_label
                                           (  etiquetas = lst_label ) ).

  APPEND LINES OF t_etiquetas TO t_etiqueta_ctt.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZCTT_ETIQUETA'
*     VARIANT            = ' '
*     DIRECT_CALL        = ' '
    IMPORTING
      fm_name            = lw_formname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc EQ 0.

*    lst_param-device    = |PRINTER|.
*    lst_param-no_dialog = abap_true.
**    lst_param-preview   = abap_true.
*
*    lst_output_opt-tdnoprev = abap_true.
*    lst_output_opt-tdimmed  = abap_true.
**    lst_output_opt-tdnoprint = abap_true.
*    lst_output_opt-tddest    = 'LOCL'.

    CALL FUNCTION lw_formname
      EXPORTING
*       ARCHIVE_INDEX    =
*       ARCHIVE_INDEX_TAB  =
*       ARCHIVE_PARAMETERS =
*       control_parameters = lst_param
*       MAIL_APPL_OBJ    =
*       MAIL_RECIPIENT   =
*       MAIL_SENDER      =
*       output_options   = lst_output_opt
*       USER_SETTINGS    = 'X'
        w_label_zpl      = pw_label
* IMPORTING
*       DOCUMENT_OUTPUT_INFO       =
*       JOB_OUTPUT_INFO  =
*       JOB_OUTPUT_OPTIONS =
      TABLES
        t_etiquetas      = t_etiqueta_ctt
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM f_top_of_page  USING dg_dyndoc_id TYPE REF TO cl_dd_document.

  "this is more clear.....check it
  "first add text, then pass it to comentry write fm
  DATA : dl_text(255) TYPE c.  "Text
* Populating header to top-of-page
***  CALL METHOD dg_dyndoc_id->add_picture
***    EXPORTING
***      picture_id      = 'KINDAMEDIO'.
**** Add new-line
***  CALL METHOD dg_dyndoc_id->new_line.

  CLEAR : dl_text.
* Move tipo de interface
  DATA(lw_interface) = COND string( WHEN p_ctt  IS NOT INITIAL THEN |CTT|
                                    WHEN p_tnb  IS NOT INITIAL THEN |TNB|
                                    WHEN p_chnr IS NOT INITIAL THEN |CHRONOPOST| ).

  CONCATENATE 'Interface :' lw_interface INTO dl_text SEPARATED BY space.
* Add User ID to Document
  PERFORM add_text USING dl_text.
* Add new-line
  CALL METHOD dg_dyndoc_id->new_line.

  CLEAR : dl_text.
* Move User ID
  CONCATENATE 'Usuário :' sy-uname INTO dl_text SEPARATED BY space.
* Add Client to Document
  PERFORM add_text USING dl_text.
* Add new-line
  CALL METHOD dg_dyndoc_id->new_line.

  CLEAR : dl_text.
* Move date
  WRITE sy-datum TO dl_text.
  CONCATENATE 'Data :' dl_text INTO dl_text SEPARATED BY space.
* Add Date to Document
  PERFORM add_text USING dl_text.
* Add new-line
  CALL METHOD dg_dyndoc_id->new_line.

  CLEAR : dl_text.
* Move time
  WRITE sy-uzeit TO dl_text.
  CONCATENATE 'Hora :' dl_text INTO dl_text SEPARATED BY space.
* Add Time to Document
  PERFORM add_text USING dl_text.
* Add new-line
  CALL METHOD dg_dyndoc_id->new_line.

* Populating data to html control
  PERFORM html.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DL_TEXT  text
*----------------------------------------------------------------------*
FORM add_text  USING pw_text TYPE sdydo_text_element.
* Adding text
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text         = pw_text
      sap_emphasis = cl_dd_area=>heading.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM html .

  DATA : dl_length        TYPE i,                           " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

* Creating html control
  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html.
  ENDIF.

* Reuse_alv_grid_commentary_set
  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->add_picture
    EXPORTING
      picture_id = 'KINDAMEDIO'.
* Add new-line
*  CALL METHOD dg_dyndoc_id->new_line.

* Get TOP->HTML_TABLE ready
  CALL METHOD dg_dyndoc_id->merge_document.

* Set wallpaper
  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

* Connect TOP document to HTML-Control
  dg_dyndoc_id->html_control = dg_html_cntrl.

* Display TOP document
  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html
    EXCEPTIONS
      html_display_error = 1.

  IF sy-subrc NE 0.
***    MESSAGE i999 WITH 'Error in displaying top-of-page'(036).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CLOSE_SHIPMENT_CTT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_close_shipment_ctt .

  DATA:
    lw_error       TYPE char1,
    t_shipclose    TYPE TABLE OF zsd_shipment2close,
    lst_shipclose  TYPE zsd_shipment2close,
    lst_log_interf TYPE zsd_log_interf.

  PERFORM f_selected_row USING lw_error.
  IF lw_error IS INITIAL.
    LOOP AT t_selected ASSIGNING FIELD-SYMBOL(<fs_selected>).

      READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_selected>-index.
      IF sy-subrc IS INITIAL AND
         <fs_saida>-tracking_num IS NOT INITIAL.

        lst_shipclose-first_object_code = <fs_saida>-tracking_num.
        lst_shipclose-last_object_code = <fs_saida>-tracking_num.
        lst_shipclose-sub_product_code = <fs_saida>-subproduct_id.
        APPEND lst_shipclose TO t_shipclose.
        CLEAR lst_shipclose.

      ENDIF.
    ENDLOOP.

    DATA(lw_rc) = w_interfaces->close_shipment_ctt( EXPORTING
                                                       t_shipclose = t_shipclose
                                                     IMPORTING
                                                       es_return = lst_return ).

    LOOP AT t_selected ASSIGNING <fs_selected>.
      READ TABLE t_saida ASSIGNING <fs_saida> INDEX <fs_selected>-index.
      IF sy-subrc IS INITIAL.
        IF lw_rc IS INITIAL.
          <fs_saida>-status = COND string( WHEN lst_return-status EQ |1| THEN icon_delivery_complete ).
          <fs_saida>-message = lst_return-message.
        ELSE.
          <fs_saida>-status = COND string( WHEN lst_return-status EQ |2| THEN icon_red_light
                                          WHEN lst_return-status EQ |3| THEN icon_yellow_light ).
          <fs_saida>-message = lst_return-message.
        ENDIF.
        MOVE-CORRESPONDING <fs_saida> TO lst_log_interf.
        lst_log_interf-status = lst_return-status.
        MODIFY zsd_log_interf FROM lst_log_interf.
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    MESSAGE text-m02 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
