class ZCL_PROCESSA_INTERFACES_TRANSP definition
  public
  final
  create public .

public section.

  methods CREATE_SHIPMENT_CTT
    importing
      !IT_XLIKP type SHP_LIKP_T
      !IT_XLIPS type SHP_LIPS_T
      !IT_XVBPA type SHP_VBPAVB_T
      !IT_XVBFA type SHP_VL10_VBFA_T
    exporting
      !ES_RETURN type ZSD_LOG_INTERF
    returning
      value(E_RC) type SY-SUBRC .
  methods CREATE_SHIPMENT_TNB
    importing
      !VBAK type VBAK
      !TVAKT type TVAKT
      !XVBAP type OI0_VBAPVB_TT
      !XVBPA type SHP_VBPAVB_T
    exporting
      !ES_RETURN type ZSD_LOG_INTERF
    returning
      value(E_RC) type SY-SUBRC .
  methods CREATE_SHIPMENT_TNB_CANCEL
    importing
      !VBAK type VBAK
      !XVBAP type OI0_VBAPVB_TT
    exporting
      !ES_RETURN type ZSD_LOG_INTERF
    returning
      value(E_RC) type SY-SUBRC .
  methods CREATE_SHIPMENT_CHRONOPOST
    importing
      !IT_XLIKP type SHP_LIKP_T
      !IT_XLIPS type SHP_LIPS_T
      !IT_XVBPA type SHP_VBPAVB_T
      !IT_XVBFA type SHP_VL10_VBFA_T
    exporting
      !ES_RETURN type ZSD_LOG_INTERF
    returning
      value(E_RC) type SY-SUBRC .
  methods CLOSE_SHIPMENT_CTT
    importing
      !T_SHIPCLOSE type ZSD_SHIPMENT2CLOSE_TAB
    exporting
      !ES_RETURN type ZSD_LOG_INTERF
    returning
      value(E_RC) type SY-SUBRC .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_PROCESSA_INTERFACES_TRANSP IMPLEMENTATION.


  METHOD close_shipment_ctt.

    DATA:
      zco_ctt_services  TYPE REF TO zco_icttshipment_provider_ws,
      lo_root           TYPE REF TO cx_root,
      lst_output        TYPE zsd_icttshipment_provider_ws_6,
      lst_input         TYPE zsd_icttshipment_provider_ws_7,
      lw_guid_16        TYPE guid_16,
      lw_guid_22        TYPE guid_22,
      lw_guid_32        TYPE guid_32,
      lw_requestid      TYPE char36,
      lst_log_interface TYPE zsd_log_interf.

    e_rc = 4.

    CREATE OBJECT zco_ctt_services.

* Fill CloseShipment Data
    lst_input-input-client_id = |11913380|.
    lst_input-input-contract_id = |300255881|.
    lst_input-input-distribution_channel = |99|.
    lst_input-input-list_of_shipment_to_close-shipment2close[] = t_shipclose[].
    lst_input-input-authentication_id = |2555a5f4-44be-43a4-96c8-5fdd9ddb85a5|.
    lst_input-input-user_id = |96cf303e-f0dc-4181-91f1-2b2be6dc1d32|.

* Create Request ID
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = lw_guid_16
        ev_guid_22 = lw_guid_22
        ev_guid_32 = lw_guid_32.
    IF sy-subrc IS INITIAL.
      lw_requestid = lw_guid_32(8) && |-| && lw_guid_32+8(4) && |-| && lw_guid_32+12(4) && |-| && lw_guid_32+16(4) && |-| && lw_guid_32+20(12).
    ENDIF.

    lst_input-input-request_id = lw_requestid.

    TRY.
        CALL METHOD zco_ctt_services->close_shipment
          EXPORTING
            input  = lst_input
          IMPORTING
            output = lst_output.

        IF lst_output-close_shipment_result-errors_list-error_data[] IS INITIAL.
          e_rc = 0.
          lst_log_interface-status = |5|.
          lst_log_interface-message = |Encomenda finalizada com sucesso|.
        ELSE.
          lst_log_interface-status = |2|.
          DATA(lst_erro) = lst_output-close_shipment_result-errors_list-error_data[ 1 ].
          lst_log_interface-message = lst_erro-message.
        ENDIF.
      CATCH cx_ai_system_fault  INTO lo_root.
        DATA(lw_text) = lo_root->get_text( ).
        lst_log_interface-status = |3|.
        lst_log_interface-message = lw_text.
    ENDTRY.

    es_return = lst_log_interface.

  ENDMETHOD.


  METHOD create_shipment_chronopost.

    DATA:
      lw_kunnr          TYPE kna1-kunnr,
      lw_brgew          TYPE menge-laeng,
      lw_weight_out     TYPE menge-laeng,
      lo_chronopost     TYPE REF TO zsd_co_services_soap,
      lo_root           TYPE REF TO cx_root,
      lst_input         TYPE zsd_registar_expedicao3soap_in,
      lst_exped         TYPE zsd_wsexpedicao2,
      lst_output        TYPE zsd_registar_expedicao3soap_ou,
      lst_log_interface TYPE zsd_log_interf,
      lw_char           TYPE char23,
      lw_dummy          TYPE char10,
      lw_idloja         TYPE zsd_b2c_pickp_t-idloja.

    e_rc = 4.

    CREATE OBJECT lo_chronopost.

    SELECT SINGLE *
     FROM zsd_param_servic
     INTO @DATA(lst_param)
     WHERE sistema EQ 'CHRONOSPOST'.

    IF sy-subrc IS INITIAL.

      lst_input-username = lst_param-username.
      lst_input-password = lst_param-password.

      TRY.
          DATA(lst_xlips) = it_xlips[ 1 ].
          IF sy-subrc IS INITIAL.

* Origem da Encomenda
            lw_kunnr = lst_xlips-werks.
            UNPACK lw_kunnr TO lw_kunnr.

            SELECT SINGLE *
            FROM kna1
            INTO @DATA(lst_kna1)
            WHERE kunnr EQ @lw_kunnr.

            IF sy-subrc IS INITIAL.

              SELECT SINGLE *
                FROM adrc
                INTO @DATA(lst_adrc_orig)
                WHERE addrnumber EQ @lst_kna1-adrnr.

              lst_exped-origem_morada_nome          = lst_kna1-name1.
              lst_exped-origem_morada_linha1        = lst_adrc_orig-mc_street.
              lst_exped-origem_morada_linha2        = lst_adrc_orig-house_num1.
              lst_exped-origem_morada_codigo_postal = lst_kna1-pstlz.
              lst_exped-origem_morada_localidade    = lst_kna1-ort01.
              lst_exped-origem_morada_pais          = lst_kna1-land1.
              lst_exped-origem_telefone             = lst_kna1-telf1.
*              lst_exped-origem_telemovel            = lst_kna1-telf2.

            ENDIF.

          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR lst_xlips.
      ENDTRY.

* Destino da Encomenda
      SELECT *
       FROM vbkd
       INTO TABLE @DATA(t_vbkd)
       FOR ALL ENTRIES IN @it_xlips
       WHERE vbeln EQ @it_xlips-vgbel.

      IF sy-subrc IS INITIAL.

        DATA(lst_vbkd) = t_vbkd[ 1 ].
        IF sy-subrc IS INITIAL.
          lw_idloja = lst_vbkd-bstkd_e.

          IF lw_idloja IS NOT INITIAL AND
             lw_idloja CO |0123456789|.

            SELECT *
              FROM zsd_b2c_pickp_t
              INTO TABLE @DATA(t_pick_point)
              WHERE idloja EQ @lw_idloja.
            IF sy-subrc IS INITIAL.
              TRY.
                  DATA(lst_pick_point) = t_pick_point[ 1 ].
                CATCH cx_sy_itab_line_not_found.
                  CLEAR lst_pick_point.
              ENDTRY.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.


      TRY.
          DATA(lst_xvbpa) = it_xvbpa[ parvw = |WE| ].
          IF sy-subrc IS INITIAL.
            SELECT SINGLE *
            FROM kna1
            INTO @DATA(lst_kna1_dest)
            WHERE kunnr EQ @lst_xvbpa-kunnr.

            IF sy-subrc IS INITIAL.

              SELECT SINGLE *
                FROM adrc
                INTO @DATA(lst_adrc)
                WHERE addrnumber EQ @lst_kna1_dest-adrnr.

              SELECT SINGLE smtp_addr
              FROM adr6
              INTO @DATA(lw_email)
              WHERE addrnumber EQ @lst_kna1_dest-adrnr.

            ENDIF.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR lst_xvbpa.
      ENDTRY.

      IF lst_pick_point IS INITIAL.
        lst_exped-tipo_destino = |1|.
        lst_exped-destino_morada_nome          = lst_kna1_dest-name1.
        lst_exped-destino_morada_linha1        = lst_adrc-mc_street.
        lst_exped-destino_morada_linha2        = lst_adrc-house_num1.
        lst_exped-destino_morada_codigo_postal = lst_kna1_dest-pstlz.
        lst_exped-destino_morada_localidade    = lst_kna1_dest-ort01.
        lst_exped-destino_morada_pais          = lst_kna1_dest-land1.
        lst_exped-destino_telefone             = lst_kna1_dest-telf1.
        lst_exped-destino_telemovel   = lst_kna1_dest-telf1.
        lst_exped-destino_email = lw_email.
      ELSE.
        lst_exped-tipo_destino = |2|.
        lst_exped-destino_loja_id              = lst_pick_point-idloja.
        SHIFT lst_exped-destino_loja_id LEFT DELETING LEADING '0'.
        lst_exped-destino_morada_nome          = lst_pick_point-nomedaloja.
        DATA(lw_size) = strlen( lst_pick_point-moradadaloja ).
        IF lw_size GT 32.
          lst_exped-destino_morada_linha1        = lst_pick_point-moradadaloja(32).
          lw_size = lw_size - 32.
          lst_exped-destino_morada_linha2        = lst_pick_point-moradadaloja+32(lw_size).
        ELSE.
          lst_exped-destino_morada_linha1        = lst_pick_point-moradadaloja.
        ENDIF.
        lst_exped-destino_morada_codigo_postal = lst_pick_point-codigopostal.
        lst_exped-destino_morada_localidade    = lst_pick_point-localidade.
        lst_exped-destino_morada_pais          = |PT|.
*        lst_exped-destino_telefone             = lst_kna1_dest-telf1.
        lst_exped-destino_contacto_nome        = lst_kna1_dest-name1.
        lst_exped-destino_contacto_telefone    = lst_kna1_dest-telf1.
        lst_exped-destino_contacto_telemovel   = lst_kna1_dest-telf1.
        lst_exped-destino_contacto_email       = lw_email.
      ENDIF.

      lw_brgew = REDUCE #( INIT lw_peso TYPE menge-laeng
                                     FOR lst_xlips_a IN it_xlips
                                       NEXT lw_peso = lw_peso + lst_xlips_a-brgew ).

* Convert weight by KG to G
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = lw_brgew
          unit_in              = 'KG'
          unit_out             = 'G'
        IMPORTING
          output               = lw_weight_out
        EXCEPTIONS
          conversion_not_found = 1
          division_by_zero     = 2
          input_invalid        = 3
          output_invalid       = 4
          overflow             = 5
          type_invalid         = 6
          units_missing        = 7
          unit_in_not_found    = 8
          unit_out_not_found   = 9
          OTHERS               = 10.


      WRITE lw_weight_out TO lw_char.
      TRANSLATE lw_char USING '. '.
      CONDENSE lw_char NO-GAPS.
      SPLIT lw_char AT ',' INTO  lst_exped-peso
                                 lw_dummy.
      TRY.
          DATA(lst_xlikp) = it_xlikp[ 1 ].
          IF sy-subrc IS INITIAL.
            IF lst_xlikp-erdat LT sy-datum.
              DATA(lw_datum) = sy-datum.
            ELSE.
              lw_datum = lst_xlikp-erdat.
            ENDIF.
            lst_exped-data_expedicao = lw_datum+6(2) && |-| && lw_datum+4(2) && |-| && lw_datum(4).
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR lst_xlikp.
      ENDTRY.

      lst_exped-enviar_etiquetas_email = abap_true.

* Checking Special Services
      SELECT *
       FROM zsd_servicos
       INTO TABLE @DATA(t_services)
       FOR ALL ENTRIES IN @it_xlips
       WHERE matnr EQ @it_xlips-matnr.
      IF sy-subrc IS INITIAL.
        DATA(lst_services) = t_services[ 1 ].
        IF sy-subrc IS INITIAL.
          lst_exped-conta = lst_services-produto_chronopost.
        ENDIF.
      ELSE.
        lst_exped-conta = |01931101|.
      ENDIF.

      lst_exped-numero_volumes = |1|.
      lst_exped-enviar_etiquetas_email = |1|.
      lst_input-expedicao = lst_exped.
      lst_input-tipo_resposta = |2|.
      lst_input-enviar_email  = |1|.
      lst_input-email = |test@hotmail.com|.
      lst_input-tipo_etiqueta = |1|.

      READ TABLE it_xvbfa INTO DATA(lst_xvbfa) INDEX 1.
      IF sy-subrc IS INITIAL.
        lst_log_interface-vbeln_va = lst_xvbfa-vbelv.
      ENDIF.

      lst_log_interface-vbeln_vl  = lst_xlikp-vbeln.
      lst_log_interface-interface = |003|.

      TRY.
          lo_chronopost->registar_expedicao3( EXPORTING
                                                input = lst_input
                                              IMPORTING
                                                 output = lst_output ).

          IF lst_output-registar_expedicao3result-codigo EQ |1|.
            e_rc = 0.
            lst_log_interface-status  = |1|.
            lst_log_interface-message = |Encomenda enviada com sucesso|.
            lst_log_interface-tracking_num = lst_output-registar_expedicao3result-nr_guia.
            lst_log_interface-link    = lst_output-registar_expedicao3result-corpo_resposta.
          ELSE.
            lst_log_interface-status = |2|.
            lst_log_interface-message = lst_output-registar_expedicao3result-descricao.
          ENDIF.

        CATCH cx_ai_system_fault INTO lo_root.
          DATA(lv_text) = lo_root->get_text( ).
          lst_log_interface-status = |3|.
          lst_log_interface-message = lv_text.
      ENDTRY.

      MODIFY zsd_log_interf FROM lst_log_interface.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.
      es_return = lst_log_interface.

    ENDIF.

  ENDMETHOD.


  METHOD create_shipment_ctt.

    DATA:
      zco_ctt_service     TYPE REF TO zco_icttshipment_provider_ws,
      lo_root             TYPE REF TO cx_root,
      lst_input           TYPE zsd_create_shipment_input,
      lst_input_ws        TYPE zsd_icttshipment_provider_ws_1,
      lst_output_ws       TYPE zsd_icttshipment_provider_ws_c,
      t_shipment_ctt      TYPE TABLE OF zshipment_ctt,
      lst_shipment_ctt    TYPE zshipment_ctt,
      lst_receiver        TYPE zaddress_data,
      lst_sender          TYPE zaddress_data,
      lst_shipment_data   TYPE zshipment_data,
      t_special_service   TYPE TABLE OF zspecial_service,
      lst_mult_home_deliv TYPE zmultiple_home_delivery_data,
      lst_time_window     TYPE ztime_window_data,
      lst_special_service TYPE zspecial_service,
      lw_kunnr            TYPE kna1-kunnr,
      lst_log_interface   TYPE zsd_log_interf,
      lw_requestid        TYPE char36,
      lw_guid_16          TYPE guid_16,
      lw_guid_22          TYPE guid_22,
      lw_guid_32          TYPE guid_32,
      lw_daynr(2)         TYPE p,
      lw_weight_in        TYPE menge-laeng,
      lw_weight_out       TYPE menge-laeng,
      lw_pstyv            TYPE tvarvc-low,
      lw_matnr            TYPE char30.

    e_rc = 4.

    CREATE OBJECT zco_ctt_service.

    lst_shipment_ctt-has_sender_information = |X|.

* fill receiver data
    READ TABLE it_xvbpa INTO DATA(lst_xvbpa)
                       WITH KEY parvw = |WE|.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
      FROM kna1
      INTO @DATA(lst_kna1)
      WHERE kunnr EQ @lst_xvbpa-kunnr.
      IF sy-subrc IS INITIAL.

        SELECT SINGLE *
          FROM adrc
          INTO @DATA(lst_adrc)
          WHERE addrnumber EQ @lst_xvbpa-adrnr.
        IF sy-subrc IS INITIAL.
          lst_receiver-name = lst_kna1-name1.
          lst_receiver-contact_name = lst_kna1-name1.
          lst_receiver-address = lst_kna1-stras.
          lst_receiver-country = lst_kna1-land1.
          lst_receiver-city    = lst_kna1-ort01.
          lst_receiver-ptzip_code_location = lst_kna1-ort01.
          lst_receiver-door    = lst_adrc-str_suppl1+1(1).
          lst_receiver-floor   = lst_adrc-str_suppl1(1).
*        lst_receiver-email   =
          lst_receiver-ptzip_code3 = lst_adrc-post_code1+5(3).
          lst_receiver-ptzip_code4 = lst_adrc-post_code1(4).
*      lst_receiver-phone       = lst_adrc-tel_number.
          lst_receiver-mobile_phone = lst_adrc-tel_number.
          lst_receiver-type        = |Receiver|.
        ENDIF.
      ENDIF.
    ENDIF.
    lst_shipment_ctt-receiver_data = lst_receiver.

* fill sender data
    TRY.
        DATA(lst_lips) = it_xlips[ 1 ].
        IF sy-subrc IS INITIAL.
          CLEAR: lst_kna1,
                lst_adrc.

          lw_kunnr =  lst_lips-werks.
          UNPACK lw_kunnr TO lw_kunnr.
          SELECT SINGLE *
           FROM kna1
           INTO lst_kna1
           WHERE kunnr EQ lw_kunnr.

          IF sy-subrc IS INITIAL.
            SELECT SINGLE *
            FROM adrc
            INTO lst_adrc
            WHERE addrnumber EQ lst_kna1-adrnr.
            IF sy-subrc IS INITIAL.
              lst_sender-name = lst_kna1-name1.
              lst_sender-address = lst_kna1-stras.
              lst_sender-country = lst_kna1-land1.
              lst_sender-city    = lst_kna1-ort01.
              lst_sender-ptzip_code_location = lst_kna1-ort01.
              lst_sender-door    = lst_adrc-str_suppl1+1(1).
              lst_sender-floor   = lst_adrc-str_suppl1(1).
              lst_sender-ptzip_code3 = lst_adrc-post_code1+5(3).
              lst_sender-ptzip_code4 = lst_adrc-post_code1(4).
*          lst_sender-phone       = lst_adrc-tel_number.
              lst_sender-type        = |Sender|.
            ENDIF.
          ENDIF.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        CLEAR lst_lips.
    ENDTRY.
    lst_shipment_ctt-sender_data = lst_sender.

* Fill shipment data
    lw_weight_in = REDUCE #( INIT val TYPE menge-laeng
                               FOR lst_xlips IN it_xlips
                                 NEXT val = val + lst_xlips-brgew ).

* Convert weight by KG to G
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = lw_weight_in
        unit_in              = 'KG'
        unit_out             = 'G'
      IMPORTING
        output               = lw_weight_out
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.

    lst_shipment_data-weight = lw_weight_out.

    DATA(lst_likp) = it_xlikp[ 1 ].
    lst_shipment_data-client_reference = lst_likp-vbeln.
    lst_shipment_data-is_devolution    = space.
    lst_shipment_data-observations     = |teste|.
    lst_shipment_data-quantity = COND string( WHEN lst_likp-anzpk IS NOT INITIAL THEN lst_likp-anzpk
                                              WHEN lst_likp-anzpk IS INITIAL THEN |1| ).
    lst_shipment_ctt-shipment_data = lst_shipment_data.

* Checking Special Services
    SELECT *
     FROM zsd_servicos
     INTO TABLE @DATA(t_servicos)
     FOR ALL ENTRIES IN @it_xlips
     WHERE matnr EQ @it_xlips-matnr.

    IF sy-subrc IS INITIAL.
      TRY.

          DATA(lst_services) = t_servicos[ 1 ].
          IF sy-subrc IS INITIAL.
            lw_matnr = lst_services-produto_ctt.
            SELECT *
             UP TO 1 ROWS
              FROM zsd_param_servic
              INTO @DATA(lst_param_serv)
              WHERE sistema EQ 'CTT'
                AND ambiente  EQ @sy-sysid
                AND username  EQ @lw_matnr.
            ENDSELECT.

            IF sy-subrc IS INITIAL.

              lst_special_service-special_service_type = lst_param_serv-password.
              IF lst_param_serv-password EQ |MultipleHomeDelivery|.
                lst_special_service-multiple_home_delivery-attempts_number = |1|.
                lst_special_service-multiple_home_delivery-in_non_delivery_case = |PostOfficeNotiffied|.
              ENDIF.
              APPEND lst_special_service TO t_special_service.

              TRY.
                  DATA(lst_xvbfa) = it_xvbfa[ 1 ].
                  SELECT SINGLE *
                    FROM vbak
                    INTO  @DATA(lst_vbak)
                    WHERE vbeln EQ @lst_xvbfa-vbelv.
                  IF sy-subrc IS INITIAL.
                    IF ( lst_vbak-zzpei IS NOT INITIAL AND lst_vbak-zzpef IS NOT INITIAL ) AND
                         lst_special_service-special_service_type EQ |MultipleHomeDelivery|.

                      SELECT SINGLE *
                        FROM zsd_timewindow
                        INTO @DATA(lst_timewin)
                        WHERE hora_inicial GE @lst_vbak-zzpei
                          AND hora_final   LT @lst_vbak-zzpef.
                      IF sy-subrc IS INITIAL.

                        CALL FUNCTION 'DAY_IN_WEEK'
                          EXPORTING
                            datum = lst_likp-erdat
                          IMPORTING
                            wotnr = lw_daynr.

                        lst_special_service-special_service_type = |TimeWindow|.
                        lst_time_window-time_window = COND string( WHEN lw_daynr NE |06| THEN lst_timewin-descricao
                                                                   WHEN lw_daynr EQ |06| THEN 'DeliverySaturday_10h00_14h00' ).
                        lst_special_service-time_window  = lst_time_window.
                        APPEND lst_special_service TO t_special_service.
                        CLEAR lst_special_service.

                      ENDIF.
                    ENDIF.
                  ENDIF.
                CATCH cx_sy_itab_line_not_found.
                  CLEAR lst_xvbfa.
              ENDTRY.

              APPEND LINES OF t_special_service TO lst_shipment_ctt-special_services-special_service.
              lst_input-delivery_note-sub_product_id = lst_services-produto_ctt.
            ELSE.
              lst_input-delivery_note-sub_product_id = lst_services-produto_ctt.
            ENDIF.
          ELSE.
            lst_input-delivery_note-sub_product_id = |EMSF056.01|.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR lst_services.
      ENDTRY.
    ELSE.
      MESSAGE 'Falta de cadastro na tabela de serviço' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

    APPEND lst_shipment_ctt TO t_shipment_ctt.
    CLEAR lst_shipment_ctt.

* Fill Delivery Note
    lst_input-delivery_note-client_id = |11913380|.
    lst_input-delivery_note-contract_id = |300255881|.
    lst_input-delivery_note-distribution_channel_id = |99|.
    lst_input-delivery_note-shipment_ctt-shipment_ctt[] = t_shipment_ctt[].
    lst_input-authentication_id = |2555a5f4-44be-43a4-96c8-5fdd9ddb85a5|.
    lst_input-user_id = |96cf303e-f0dc-4181-91f1-2b2be6dc1d32|.

* Create Request ID
    CALL FUNCTION 'GUID_CREATE'
      IMPORTING
        ev_guid_16 = lw_guid_16
        ev_guid_22 = lw_guid_22
        ev_guid_32 = lw_guid_32.
    IF sy-subrc IS INITIAL.
      lw_requestid = lw_guid_32(8) && |-| && lw_guid_32+8(4) && |-| && lw_guid_32+12(4) && |-| && lw_guid_32+16(4) && |-| && lw_guid_32+20(12).
    ENDIF.

    lst_input-request_id = lw_requestid.

    lst_input_ws-input = lst_input.

    lst_log_interface-interface = |002|.

    READ TABLE it_xvbfa INTO lst_xvbfa INDEX 1.
    IF sy-subrc IS INITIAL.
      lst_log_interface-vbeln_va = lst_xvbfa-vbelv.
    ENDIF.
    lst_log_interface-subproduct_id = lst_input-delivery_note-sub_product_id.
    lst_log_interface-vbeln_vl  = lst_likp-vbeln.

* Call interface create shipment on CTT
    TRY.
        CALL METHOD zco_ctt_service->create_shipment(
          EXPORTING
            input  = lst_input_ws
          IMPORTING
            output = lst_output_ws ).

      CATCH cx_ai_system_fault INTO lo_root.
        DATA(lv_text) = lo_root->get_text( ).
        lst_log_interface-status = |3|.
        lst_log_interface-message = lv_text.
    ENDTRY.

    IF lst_output_ws-create_shipment_result-status EQ |Success|.
      DATA(lst_shipment_list) = lst_output_ws-create_shipment_result-shipment_data-shipment_data_output[ 1 ].
      DATA(lst_label) = lst_shipment_list-label_list-label_data[ 1 ].
      lst_log_interface-status = |1|.
      lst_log_interface-tracking_num = lst_shipment_list-first_object.
      lst_log_interface-link  = lst_label-label.
      lst_log_interface-message = |Dados enviados com sucesso|.
      e_rc = 0.
    ELSEIF lst_output_ws-create_shipment_result-status EQ |Failure|.
      lst_log_interface-status = |2|.
      DATA(lst_error) = lst_output_ws-create_shipment_result-errors_list-error_data[ 1 ].
      lst_log_interface-message = lst_error-message.
    ENDIF.

    MODIFY zsd_log_interf FROM lst_log_interface.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.
    es_return = lst_log_interface.

  ENDMETHOD.


  method create_shipment_tnb.

    types: begin of ty_kna1,
             kunnr type kna1-kunnr,
             stras type kna1-stras,
             land1 type kna1-land1,
             stcd1 type kna1-stcd1,
             ort01 type kna1-ort01,
             name1 type kna1-name1,
             pstlz type kna1-pstlz,
             telf1 type kna1-telf1,
             telf2 type kna1-telf2,
             adrnr type kna1-adrnr,
           end of ty_kna1,

           begin of ty_t005t,
             land1 type t005t-land1,
             landx type t005t-landx,
             spras type t005t-spras,
           end of ty_t005t,

           begin of ty_adr6,
             addrnumber type adr6-addrnumber,
             smtp_addr  type adr6-smtp_addr,
           end of ty_adr6,

           begin of ty_tnb,
             matnr       type zsd_servicos-matnr,
             produto_tnb type zsd_servicos-produto_tnb,
           end of ty_tnb.

    data: t_kna1  type table of ty_kna1,
          t_t005t type table of ty_t005t,
          t_adr6  type table of ty_adr6,
          t_tnb   type table of ty_tnb.

    data: lo_trtnb_servs     type ref to zsd_co_ii_moovit_web_service_c.
    data: lo_root            type ref to cx_root.

    data: ls_auth_in       type zsd_ii_moovit_web_service_co11,
          ls_auth_out      type zsd_ii_moovit_web_service_co10,
          ls_create_in     type zsd_ii_moovit_web_service_con7,
          ls_create_out    type zsd_ii_moovit_web_service_con6,
          ls_log_interface type zsd_log_interf.

    data: r_pstyv  type range of vbap-pstyv,
          ls_pstyv like line of r_pstyv.

    constants: c_serv  type c length 3 value 'TNB',
               c_nserv type c length 3 value '01'.

    data: ls_articles type zsd_service_article,
          ls_srvenum  type zsd_article_service_type_enum,
          ls_adress   type zsd_service_address.
    data: t_vbap type oi0_vbapvb_tt.

    data: lv_vol_in  type vbap-volum,
          lv_vol_out type vbap-volum,
          lv_volum   type t006-msehi.
    "-----------------------------------------------------------------------

    clear: ls_create_out, ls_create_in, ls_auth_in, ls_auth_in.
    clear: t_vbap[], t_kna1[], t_t005t[], t_adr6[], t_tnb[].



    create object lo_trtnb_servs.

    select kunnr stras land1 stcd1 ort01
           name1 pstlz telf1 telf2 adrnr
    from kna1
    into table t_kna1
    where kunnr = vbak-kunnr.

    if sy-subrc = 0.
      sort t_kna1 by kunnr.
      select addrnumber smtp_addr
      from adr6
      into table t_adr6
      for all entries in t_kna1
      where addrnumber = t_kna1-adrnr.

      if sy-subrc = 0.
        sort t_adr6 by addrnumber.
      endif.

      select land1 landx spras
      from t005t
      into table t_t005t
      for all entries in t_kna1
      where land1 = t_kna1-land1
        and spras = sy-langu.

      if sy-subrc = 0.
        sort t_t005t by land1.
      endif.
    endif.

    select matnr produto_tnb
    from zsd_servicos
    into table t_tnb
    for all entries in xvbap
    where matnr = xvbap-matnr.

    if sy-subrc = 0.
      sort t_tnb by matnr.
    endif.

    select single username password
    from zsd_param_servic
    into (ls_auth_in-username, ls_auth_in-password)
    where sistema    = c_serv
      and servico    = c_nserv.

    select  *
    from tvarvc
    into table @data(t_tvarvc)
    where name eq 'ZSD_ITEM_CATEG_TNB'.

    if sy-subrc = 0.
      loop at t_tvarvc into data(ls_tvarv).
        ls_pstyv-sign = 'I'.
        if not ls_tvarv-high is initial.
          ls_pstyv-option = 'BT'.
          ls_pstyv-high = ls_tvarv-high.
        else.
          ls_pstyv-option = 'EQ'.
        endif.
        ls_pstyv-low = ls_tvarv-low.
        append ls_pstyv to r_pstyv.
        clear ls_pstyv.
      endloop.
    endif.

    select matnr, attyp
    from mara
    into table @data(lt_mara)
    for all entries in @xvbap
    where matnr = @xvbap-matnr.
    if sy-subrc = 0.
      sort lt_mara by matnr.

      select matnr, werks, stlnr
      from mast
      into table @data(lt_mast)
      for all entries in @xvbap
      where matnr = @xvbap-matnr
        and werks = @xvbap-werks.
      if sy-subrc = 0.
        sort lt_mast by matnr werks.

        select stlnr, stlkn
        from stpo
        into table @data(lt_stpo)
        for all entries in @lt_mast
        where stlnr = @lt_mast-stlnr.
        if sy-subrc = 0.
          sort lt_stpo by stlnr.
        endif.
      endif.
    endif.

    data(lt_stpo_aux) = lt_stpo.

    t_vbap[] = xvbap[].
    sort t_vbap by uepos.

    loop at xvbap assigning field-symbol(<fs_vbap>).
      if <fs_vbap>-pstyv in r_pstyv.
        read table xvbpa assigning field-symbol(<fs_vbpa>) with key
        parvw = 'RG'.

        if sy-subrc  = 0.
          ls_create_in-service_request-billing_address-additional_instructions =
          <fs_vbpa>-kunnr.

          read table t_kna1 into data(ls_kna1) with key kunnr = <fs_vbpa>-kunnr
                                                                 binary search.

          if sy-subrc = 0.
            ls_create_in-service_request-billing_address-address       = ls_kna1-stras.
            ls_create_in-service_request-billing_address-fiscal_number = ls_kna1-stcd1.
            ls_create_in-service_request-billing_address-location      = ls_kna1-ort01.
            ls_create_in-service_request-billing_address-name          = ls_kna1-name1.
*ls_create_in-service_request-billing_address-postal_code      =
*ls_kna1-pstlz.
            ls_create_in-service_request-billing_address-postal_code   = '4000'.
            ls_create_in-service_request-billing_address-primary_phone = ls_kna1-telf1.

            if ls_kna1-telf2 is initial.
              ls_create_in-service_request-billing_address-secondary_phone = ls_kna1-telf1.
            else.
              ls_create_in-service_request-billing_address-secondary_phone = ls_kna1-telf2.
            endif.

            read table t_t005t into data(ls_005) with key land1 = ls_kna1-land1.
            if sy-subrc  = 0.
              ls_create_in-service_request-billing_address-country = ls_005-landx.
            endif.
            read table t_adr6 into data(ls_adr6) with key addrnumber = ls_kna1-adrnr
                                                                      binary search.
            if sy-subrc = 0.
              ls_create_in-service_request-billing_address-email  =
              ls_adr6-smtp_addr .
            endif.
          endif.
        endif.

        read table xvbpa assigning <fs_vbpa> with key parvw = 'WE'.
        if sy-subrc = 0.
          ls_create_in-service_request-delivery_address-additional_instructions =
                    <fs_vbpa>-kunnr.

          read table t_kna1 into ls_kna1 with key kunnr = <fs_vbpa>-kunnr
                                                           binary search.

          if sy-subrc = 0.
            ls_create_in-service_request-delivery_address-address       = ls_kna1-stras.
            ls_create_in-service_request-delivery_address-fiscal_number = ls_kna1-stcd1.
            ls_create_in-service_request-delivery_address-location      = ls_kna1-ort01.
            ls_create_in-service_request-delivery_address-name          = ls_kna1-name1.
*ls_create_in-service_request-delivery_address-postal_code      =
*ls_kna1-pstlz.
            ls_create_in-service_request-delivery_address-postal_code   = '4000'.
            ls_create_in-service_request-delivery_address-primary_phone = ls_kna1-telf1.

            if ls_kna1-telf2 is initial.
              ls_create_in-service_request-delivery_address-secondary_phone  = ls_kna1-telf1.
            else.
              ls_create_in-service_request-delivery_address-secondary_phone  = ls_kna1-telf2.
            endif.

            read table t_t005t into ls_005 with key land1 = ls_kna1-land1.
            if sy-subrc  = 0.
              ls_create_in-service_request-delivery_address-country = ls_005-landx.
            endif.

            read table t_adr6 into ls_adr6 with key addrnumber = ls_kna1-adrnr
                                                                binary search.
            if sy-subrc = 0.
              ls_create_in-service_request-delivery_address-email  = ls_adr6-smtp_addr .
            endif.
          endif.
        endif.

        ls_articles-article_id      = <fs_vbap>-matnr.
        ls_articles-description     = <fs_vbap>-arktx.
        ls_articles-quantity        = <fs_vbap>-kwmeng.
        ls_articles-reference_code  = vbak-vbeln.

        ls_articles-unit_packages   = 0.

        try.
            data(ls_mara) = lt_mara[ matnr = <fs_vbap>-matnr ].

            if ls_mara-attyp = '10'.

              try.
                  data(ls_mast) = lt_mast[ matnr = <fs_vbap>-matnr
                                           werks = <fs_vbap>-werks ].

                  lt_stpo_aux[] = lt_stpo[].

                  delete lt_stpo_aux where stlnr <> ls_mast-stlnr.
                  describe table lt_stpo_aux lines data(lv_lines).

                  ls_articles-unit_packages = lv_lines.
                catch cx_sy_itab_line_not_found.
                  clear ls_mast.
              endtry.

            elseif ls_mara-attyp = '00'.
              lv_lines = 1.
              ls_articles-unit_packages = 1.
            elseif ls_mara-attyp = '12'.
              lv_lines = 1.
              ls_articles-unit_packages = 1.
            endif.

          catch cx_sy_itab_line_not_found.
            clear ls_mara.
        endtry.

        if not <fs_vbap>-kwmeng lt 0.
          ls_articles-unit_price_euro = <fs_vbap>-netwr / <fs_vbap>-kwmeng.
        endif.

          lv_volum = <fs_vbap>-voleh.

          lv_vol_in = <fs_vbap>-volum.
          call function 'UNIT_CONVERSION_SIMPLE'
            exporting
              input                = lv_vol_in
              unit_in              = lv_volum
              unit_out             = 'M3'
            importing
              output               = lv_vol_out
            exceptions
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              others               = 10.

          if sy-subrc <> 0.
* Implement suitable error handling here
          endif.


        ls_articles-unit_volume_m3  = lv_vol_out.
        ls_articles-unit_weight_kg  = <fs_vbap>-brgew.

        clear ls_create_in-service_request-service_type.

        ls_srvenum = 'Delivery'.
        append ls_srvenum to
        ls_articles-services-article_service_type_enum.

        clear ls_srvenum.


        if ls_create_in-service_request-service_type is initial.
          read table t_tnb into data(ls_tnb) with key produto_tnb = 'ExpressDelivery'.
          if sy-subrc  = 0.
            read table t_tnb into ls_tnb with key produto_tnb = 'Assembly'.
            if sy-subrc = 0.
              ls_create_in-service_request-service_type = 'ExpressDeliveryWithAssembly'.
            else.
              ls_create_in-service_request-service_type = 'ExpressDeliveryWithoutAssembly'.
            endif.
          else.
            read table t_tnb into ls_tnb with key produto_tnb = 'Assembly'.
            if sy-subrc = 0.
              ls_create_in-service_request-service_type = 'DeliveryWithAssembly'.
            else.
              ls_create_in-service_request-service_type = 'DeliveryWithoutAssembly'.
            endif.

          endif.
        endif.

        read table t_vbap into data(ls_vbap) with key uepos = <fs_vbap>-posnr.
        if sy-subrc = 0.

          read table t_tnb into ls_tnb with key matnr = ls_vbap-matnr.
          if sy-subrc = 0.

            ls_srvenum = ls_tnb-produto_tnb.
            append ls_srvenum to
            ls_articles-services-article_service_type_enum.

          endif.

        endif.

        append ls_articles to
        ls_create_in-service_request-articles-service_article.
        clear ls_articles.

        ls_create_in-service_request-request_id   = vbak-vbeln.

        ls_create_in-service_request-store_id     = '158'.

**      CONCATENATE vbak-erdat(4)
**                  vbak-erdat+4(2)
**                  vbak-erdat+6(2)
**             INTO ls_create_in-service_request-time_window-date
**             SEPARATED BY '-'.

        ls_create_in-service_request-time_window-date = '2018-11-06'.

**      CONCATENATE vbak-erzet(2)
**                  vbak-erzet+2(2)
**             INTO ls_create_in-service_request-time_window-start_hour
**             SEPARATED BY ':'.

        ls_create_in-service_request-time_window-start_hour = '14:00'.

**      DATA(lv_erzet) = vbak-erzet.
**
**      IF lv_erzet(2) LT '23'.
**        lv_erzet(2) = lv_erzet(2) + 1.
**      ENDIF.
**
**      CONCATENATE lv_erzet(2)
**                  lv_erzet+2(2)
**             INTO ls_create_in-service_request-time_window-end_hour
**             SEPARATED BY ':'.

        ls_create_in-service_request-time_window-end_hour = '18:00'.
      endif.
    endloop.

    if not ls_create_in is initial.

      try.
          call method lo_trtnb_servs->authenticate
            exporting
              input  = ls_auth_in
            importing
              output = ls_auth_out.

        catch cx_ai_system_fault into lo_root.
          data(lv_text) = lo_root->get_text( ).
          ls_create_out-create_service_request_result-additional_info =
          vbak-vbeln.
          ls_create_out-create_service_request_result-code            =
          lv_text.
      endtry.

      ls_create_in-authentication_ticket =
      ls_auth_out-authenticate_result-authentication_ticket.

      try.
          call method lo_trtnb_servs->create_service_request
            exporting
              input  = ls_create_in
            importing
              output = ls_create_out.

        catch cx_ai_system_fault into lo_root.
          lv_text = lo_root->get_text( ).

          ls_log_interface-status    = |2|.
          ls_log_interface-message   = lv_text.
          ls_log_interface-interface = '001'.
          ls_log_interface-vbeln_va  = vbak-vbeln.
      endtry.

      if lv_text is initial.

        if ls_create_out-create_service_request_result-code = 'Success'.
          ls_log_interface-status = |1|.
          ls_log_interface-tracking_num  = vbak-vbeln.
        else.
          ls_log_interface-status = |2|.
        endif.

        ls_log_interface-message =
        ls_create_out-create_service_request_result-additional_info.
        e_rc = 0.
        ls_log_interface-interface = |001|.
        ls_log_interface-vbeln_va  = vbak-vbeln.

      endif.

      clear: ls_create_out, ls_create_in, ls_auth_in, ls_auth_in.

      modify zsd_log_interf from ls_log_interface.
      es_return = ls_log_interface.

    endif.

  endmethod.


  method create_shipment_tnb_cancel.

    types: begin of ty_tvagt,
             spras type tvagt-spras,
             abgru type tvagt-abgru,
             bezei type tvagt-bezei,
           end of ty_tvagt.

    data: t_tvagt type table of ty_tvagt.

    data: ls_auth_in       type zsd_ii_moovit_web_service_co11,
          ls_auth_out      type zsd_ii_moovit_web_service_co10,
          ls_cancel_in     type zsd_ii_moovit_web_service_con9,
          ls_cancel_out    type zsd_ii_moovit_web_service_con8,
          ls_log_interface type zsd_log_interf.

    constants: c_serv  type c length 3 value 'TNB',
               c_nserv type c length 3 value '03'.

    data: lo_trtnb_servs type ref to zsd_co_ii_moovit_web_service_c.
    data: lo_root        type ref to cx_root.

    "---------------------------------------------------------------------------


    select spras
           abgru
           bezei
    from tvagt
    into table t_tvagt
    for all entries in xvbap
    where abgru = xvbap-abgru
      and spras = sy-langu.

    if sy-subrc = 0.
      sort t_tvagt by abgru.
    endif.


    select single username
              password
    from zsd_param_servic
    into (ls_auth_in-username, ls_auth_in-password)
    where sistema    = c_serv
      and servico    = c_nserv.

    create object lo_trtnb_servs.

    read table xvbap assigning field-symbol(<fs_vbap>) with key vbeln = vbak-vbeln.
    if sy-subrc = 0.

      read table t_tvagt assigning field-symbol(<fs_tvagt>) with key abgru = <fs_vbap>-abgru.
      if sy-subrc = 0.
        ls_cancel_in-cancel_request-cancel_reason      = <fs_tvagt>-bezei.
        ls_cancel_in-cancel_request-service_request_id = vbak-vbeln.
        ls_cancel_in-cancel_request-store_id           = '158'."<fs_vbap>-werks.
      endif.

      if not ls_cancel_in is initial.

        try.
            call method lo_trtnb_servs->authenticate
              exporting
                input  = ls_auth_in
              importing
                output = ls_auth_out.

          catch cx_ai_system_fault into lo_root.
            data(lv_text) = lo_root->get_text( ).
            ls_cancel_out-cancel_service_request_result-additional_info = vbak-vbeln.
            ls_cancel_out-cancel_service_request_result-code            = lv_text.
        endtry.

        ls_cancel_in-authentication_ticket = ls_auth_out-authenticate_result-authentication_ticket.

        try.
            call method lo_trtnb_servs->cancel_service_request
              exporting
                input  = ls_cancel_in
              importing
                output = ls_cancel_out.

          catch cx_ai_system_fault into lo_root.
            lv_text = lo_root->get_text( ).

            ls_log_interface-status = |2|.
            ls_log_interface-message = lv_text.

        endtry.

        if ls_cancel_out-cancel_service_request_result-additional_info cs 'Sucesso'.
          ls_log_interface-status = |1|.
          ls_log_interface-tracking_num  = vbak-vbeln.
          ls_log_interface-prod_cancel   = abap_true.
**        ELSE.
**          ls_log_interface-status = |2|.
        endif.
**
**        ls_log_interface-message = ls_cancel_out-cancel_service_request_result-additional_info.
**        e_rc = 0.
**        ls_log_interface-interface = '001'.
**        ls_log_interface-vbeln_va  = vbak-vbeln.


        modify zsd_log_interf from ls_log_interface.
        es_return = ls_log_interface.

        clear: ls_cancel_out, ls_cancel_in, ls_auth_in, ls_auth_in.
      endif.
    endif.

  endmethod.
ENDCLASS.
