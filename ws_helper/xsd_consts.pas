{
    This unit is part of the Web Service Toolkit
    Copyright (c) 2006 by Inoussa OUEDRAOGO

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}
{$INCLUDE wst_global.inc}
unit xsd_consts;

interface

const
  s_address                    : WideString = 'address';
  s_all                        : WideString = 'all';
  //s_any                        : WideString = 'any';
  s_annotation                 : WideString = 'annotation';
  s_appinfo                    : WideString = 'appinfo';
  s_array                      : WideString = 'array';
  s_arrayType                  : WideString = 'arrayType';
  s_attribute                  : WideString = 'attribute';
  s_base                       : WideString = 'base';
  s_binding                    : WideString = 'binding';
  s_body                       : WideString = 'body';
  s_complexContent             : WideString = 'complexContent';
  s_complexType                : WideString = 'complexType';
  s_customAttributes           : WideString = 'customAttributes';
  s_definitions                             = 'definitions';
  s_document                   : WideString = 'document';
  s_element                    : WideString = 'element';
  s_enumeration                : WideString = 'enumeration';
  s_extension                  : WideString = 'extension';
  s_guid                       : WideString = 'GUID';
  s_input                      : WideString = 'input';
  s_item                       : WideString = 'item';
  s_literal                                 = 'literal';
  s_location                   : WideString = 'location';
  s_message                    : WideString = 'message';
  s_maxOccurs                  : WideString = 'maxOccurs';
  s_minOccurs                  : WideString = 'minOccurs';
  s_name                       : WideString = 'name';
  s_namespace                               = 'namespace';
  s_operation                               = 'operation';
  s_optional                   : WideString = 'optional';
  s_output                     : WideString = 'output';
  s_part                       : WideString = 'part';
  s_port                       : WideString = 'port';
  s_portType                                = 'portType';
  s_prohibited                              = 'prohibited';

  s_ref                        : WideString = 'ref';
  s_required                   : WideString = 'required';
  s_restriction                : WideString = 'restriction';
  //s_return                     : WideString = 'return';
  s_rpc                                     = 'rpc';
  s_schema                     : WideString = 'schema';
  s_xs                         : WideString = 'http://www.w3.org/2001/XMLSchema';
  s_xs_short                                = 'xsd';
  s_sequence                   : WideString = 'sequence';
  s_service                    : WideString = 'service';
  s_simpleContent              : WideString = 'simpleContent';
  s_simpleType                 : WideString = 'simpleType';
  s_soap                       : WideString = 'http://schemas.xmlsoap.org/wsdl/soap/';
  s_soap_short_name                         = 'soap';
  s_soapAction                              = 'soapAction';
  s_soapInputEncoding                       = 'Input_EncodingStyle';
  s_soapOutputEncoding                      = 'OutputEncodingStyle';
  s_soapStyle                               = 'style';
  s_soapTransport                           = 'http://schemas.xmlsoap.org/soap/http';
  s_style                                   = 'style';
  s_targetNamespace                         = 'targetNamespace';
  s_tns                                     = 'tns';
  s_transport                               = 'transport';
  s_type                                    = 'type';
  s_types                                   = 'types';
  s_unbounded                               = 'unbounded';
  s_use                                     = 'use';
  s_value                                   = 'value';
  s_wsdl                                    = 'http://schemas.xmlsoap.org/wsdl/';
  s_xmlns                                   = 'xmlns';

  
  s_WST_headerBlock             = 'wst_headerBlock';
  s_WST_record                  = 'wst_record';
  s_WST_storeType               = 'StoreType';
  
implementation

end.
