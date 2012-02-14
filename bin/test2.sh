#!/bin/bash
create_xml()
{
echo "<soap-env:Envelope xmlns:soap-env=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/1999/XMLSchema-instance\">
        <soap-env:Header>
                <NS1:MTOSI_Header xmlns:NS1=\"tmf854.v1\">
                        <NS1:domain>Configuration</NS1:domain>
                        <NS1:activityName>ServiceCreation</NS1:activityName>
                        <NS1:msgName>addFPsToFDFr</NS1:msgName>
                        <NS1:msgType>REQUEST</NS1:msgType>
                        <NS1:payloadVersion>1.0</NS1:payloadVersion>
                        <NS1:correlationId>20110330010175046045000000002814842847</NS1:correlationId>
                        <NS1:communicationPattern>SimpleResponse</NS1:communicationPattern>
                        <NS1:communicationStyle>MSG</NS1:communicationStyle>
                        <NS1:requestedBatchSize>0</NS1:requestedBatchSize>
                        <NS1:batchSequenceNumber>0</NS1:batchSequenceNumber>
                        <NS1:batchSequenceEndOfReply>false</NS1:batchSequenceEndOfReply>
                        <NS1:timestamp>2011-03-30T20:28:27.977+0200</NS1:timestamp>
                        <NS1:vendorExtensions>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">NetzarchitekturTyp=ZAMP</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">VMAC_ptpNm=/rack=1/shelf=1/slot=3/sub_slot=0/port=2</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">scope=STANDARD</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">entryPoint=20060803164028010072000000000006590995.DSLAM_xDSLPort</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">produktId=2</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">pvar=PremiumPlus</string>
                                <ns1:NameAndStringValue xmlns:ns1=\"http://telekom.de/flexprod/common\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"ns1:NameAndStringValue\">
                                        <ns1:name>UserID</ns1:name>
                                        <ns1:value>fxp_mtosi</ns1:value>
                                </ns1:NameAndStringValue>
                        </NS1:vendorExtensions>
                </NS1:MTOSI_Header>
        </soap-env:Header>
        <soap-env:Body>
                <NS2:addFPsToFDFr xmlns:NS2=\"tmf854.v1\">
                        <NS2:fdfrName>
                                <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                <NS2:fdNm>AGS-Speicherleck-Analyse_E.Jenn</NS2:fdNm>
                        <NS2:fdfrNm>ServiceCreation</NS2:fdfrNm>
                </NS2:fdfrName>
                <NS2:tpNames>
                        <NS2:name>
                                <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10
od </NS2:ptpNm>
                                <NS2:ctpNm>/svid=$VLAN/cvid=7</NS2:ctpNm>
                        </NS2:name>
                        <NS2:name>
                                <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10</NS2:ptpNm>
                                <NS2:ctpNm>/svid=$VLAN/cvid=8</NS2:ctpNm>
                        </NS2:name>
                </NS2:tpNames>
                <NS2:tpsToModify>
                        <NS2:tpData>
                                <NS2:tpName>
                                        <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                        <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                        <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10</NS2:ptpNm>
                                        <NS2:ctpNm>/svid=$VLAN/cvid=7</NS2:ctpNm>
                                </NS2:tpName>
                                <NS2:transmissionParams>
                                        <NS2:layeredParameters>
                                                <NS2:layer>LR_Ethernet</NS2:layer>
                                                <NS2:vendorExtensions>
                                                        <vendorExtensions xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"vendorExtensions\">
                                                                <NameAndStringValue>
                                                                        <name>ServiceProfile</name>
                                                                        <value>PACKET/m_AGS1_UNI_16704to25088_1600to5056_PG02</value>
                                                                </NameAndStringValue>
                                                                <NameAndStringValue>
                                                                        <name>ISP_LABEL</name>
                                                                        <value>$ISPSTR_FP3</value>
                                                                </NameAndStringValue>
                                                        </vendorExtensions>
                                                </NS2:vendorExtensions>
                                        </NS2:layeredParameters>
                                </NS2:transmissionParams>
                        </NS2:tpData>
                        <NS2:tpData>
                                <NS2:tpName>
                                        <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                        <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                        <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10</NS2:ptpNm>
                                        <NS2:ctpNm>/svid=$VLAN/cvid=8</NS2:ctpNm>
                                </NS2:tpName>
                                <NS2:transmissionParams>
                                        <NS2:layeredParameters>
                                                <NS2:layer>LR_Ethernet</NS2:layer>
                                                <NS2:vendorExtensions>
                                                        <vendorExtensions xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"vendorExtensions\">
                                                                <NameAndStringValue>
                                                                        <name>ServiceProfile</name>
                                                                        <value>PACKET/z_AGS1_UNI_IPTV_16704to25088_1600to5056_PG02</value>
                                                                </NameAndStringValue>
                                                                <NameAndStringValue>
                                                                        <name>ISP_LABEL</name>
                                                                        <value>$ISPSTR_FP14</value>
                                                                </NameAndStringValue>
                                                        </vendorExtensions>
                                                </NS2:vendorExtensions>
                                        </NS2:layeredParameters>
                                </NS2:transmissionParams>
                        </NS2:tpData>
                </NS2:tpsToModify>
        </NS2:addFPsToFDFr>
        </soap-env:Body>
</soap-env:Envelope>"
}
 
delete_xml()
{
echo "<soap-env:Envelope xmlns:soap-env=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/1999/XMLSchema-instance\">
        <soap-env:Header>
                <NS1:MTOSI_Header xmlns:NS1=\"tmf854.v1\">
                        <NS1:domain>Configuration</NS1:domain>
                        <NS1:activityName>ServiceCreation</NS1:activityName>
                        <NS1:msgName>removeFPsFromFDFr</NS1:msgName>
                        <NS1:msgType>REQUEST</NS1:msgType>
                        <NS1:payloadVersion>1.0</NS1:payloadVersion>
                        <NS1:correlationId>20110330010175046045000000002814842847</NS1:correlationId>
                        <NS1:communicationPattern>SimpleResponse</NS1:communicationPattern>
                        <NS1:communicationStyle>MSG</NS1:communicationStyle>
                        <NS1:requestedBatchSize>0</NS1:requestedBatchSize>
                        <NS1:batchSequenceNumber>0</NS1:batchSequenceNumber>
                        <NS1:batchSequenceEndOfReply>false</NS1:batchSequenceEndOfReply>
                        <NS1:timestamp>2011-03-30T20:28:27.977+0200</NS1:timestamp>
                        <NS1:vendorExtensions>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">NetzarchitekturTyp=ZAMP</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">VMAC_ptpNm=/rack=1/shelf=1/slot=3/sub_slot=0/port=2</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">scope=STANDARD</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">entryPoint=20060803164028010072000000000006590995.DSLAM_xDSLPort</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">produktId=2</string>
                                <string xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"java:java.lang.String\">pvar=PremiumPlus</string>
                                <ns1:NameAndStringValue xmlns:ns1=\"http://telekom.de/flexprod/common\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"ns1:NameAndStringValue\">
                                        <ns1:name>UserID</ns1:name>
                                        <ns1:value>fxp_mtosi</ns1:value>
                                </ns1:NameAndStringValue>
                        </NS1:vendorExtensions>
                </NS1:MTOSI_Header>
        </soap-env:Header>
        <soap-env:Body>
                <NS2:removeFPsFromFDFr xmlns:NS2=\"tmf854.v1\">
                        <NS2:fdfrName>
                                <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                <NS2:fdNm>AGS-Speicherleck-Analyse_E.Jenn</NS2:fdNm>
                        <NS2:fdfrNm>ServiceDeletion</NS2:fdfrNm>
                </NS2:fdfrName>
                <NS2:tpNames>
                        <NS2:name>
                                <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10</NS2:ptpNm>
                                <NS2:ctpNm>/svid=$VLAN/cvid=7</NS2:ctpNm>
                        </NS2:name>
                        <NS2:name>
                                <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10</NS2:ptpNm>
                                <NS2:ctpNm>/svid=$VLAN/cvid=8</NS2:ctpNm>
                        </NS2:name>
                </NS2:tpNames>
                <NS2:tpsToRemove>
                        <NS2:tpData>
                                <NS2:tpName>
                                        <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                        <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                        <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10</NS2:ptpNm>
                                        <NS2:ctpNm>/svid=$VLAN/cvid=7</NS2:ctpNm>
                                </NS2:tpName>
                                <NS2:transmissionParams>
                                        <NS2:layeredParameters>
                                                <NS2:layer>LR_Ethernet</NS2:layer>
                                                <NS2:vendorExtensions>
                                                        <vendorExtensions xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"vendorExtensions\">
                                                                <NameAndStringValue>
                                                                        <name>ServiceProfile</name>
                                                                        <value>PACKET/m_AGS1_UNI_16704to25088_1600to5056_PG02</value>
                                                                </NameAndStringValue>
                                                                <NameAndStringValue>
                                                                        <name>ISP_LABEL</name>
                                                                        <value>$ISPSTR_FP3</value>
                                                                </NameAndStringValue>
                                                        </vendorExtensions>
                                                </NS2:vendorExtensions>
                                        </NS2:layeredParameters>
                                </NS2:transmissionParams>
                        </NS2:tpData>
                        <NS2:tpData>
                                <NS2:tpName>
                                        <NS2:mdNm>Ericsson/SOI_GPON</NS2:mdNm>
                                        <NS2:meNm>Vasily_AGSR8</NS2:meNm>
                                        <NS2:ptpNm>/rack=1/shelf=1/slot=1/sub_slot=1/port=10</NS2:ptpNm>
                                        <NS2:ctpNm>/svid=$VLAN/cvid=8</NS2:ctpNm>
                                </NS2:tpName>
                                <NS2:transmissionParams>
                                        <NS2:layeredParameters>
                                                <NS2:layer>LR_Ethernet</NS2:layer>
                                                <NS2:vendorExtensions>
                                                        <vendorExtensions xmlns=\"\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"vendorExtensions\">
                                                                <NameAndStringValue>
                                                                        <name>ServiceProfile</name>
                                                                        <value>PACKET/z_AGS1_UNI_IPTV_16704to25088_1600to5056_PG02</value>
                                                                </NameAndStringValue>
                                                                <NameAndStringValue>
                                                                        <name>ISP_LABEL</name>
                                                                        <value>$ISPSTR_FP14</value>
                                                                </NameAndStringValue>
                                                        </vendorExtensions>
                                                </NS2:vendorExtensions>
                                        </NS2:layeredParameters>
                                </NS2:transmissionParams>
                        </NS2:tpData>
                </NS2:tpsToRemove>
        </NS2:removeFPsFromFDFr>
        </soap-env:Body>
</soap-env:Envelope>"
}
 
# VLAN="128"
MATNR="40236122"
DSLAM="49_777_7_71E7"
ISPSTR_FP3=""
ISPSTR_FP14="DSLAM_ZDCN_IP=192.10.10.49;DSLAM_SLOT=10;DSLAM_PORT=11;"
COUNT=1
 
for ((VLAN=128;VLAN<1128;VLAN++))
do
  echo "$(date "+%Y_%m_%d/%X") Loesche VLAN $VLAN (Count=$COUNT)"
  create_xml > /tmp/xml.xml
  # delete_xml > /tmp/xml.xml
  /opt/marconi/soi-mtosi/bin/HttpClient http://localhost:12347/ /tmp/xml.xml >/dev/null
  COUNT=$(( $COUNT + 1 ))
done
 

