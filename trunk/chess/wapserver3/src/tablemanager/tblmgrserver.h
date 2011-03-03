/******************************************************************************
 *
 *       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
 *
 *       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
 *       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
 *       WapPortal.RU Ltd.
 *
 * Filename:     $Source: /home/cvs/wapserver3/tablemanager/tblmgrserver.h,v $
 * Author:       $Author: leha $
 * Locker:       $Locker:  $
 * State:        $State: Exp $
 * Revision:     $Revision: 1.4 $
 *
 * History:      Use the CVS command log to display revision history
 *               information.
 *
 * Description:
 *
 * Notes:
 *
 ******************************************************************************/
#ifndef _TBLMGRSERVER_H
#define _TBLMGRSERVER_H

#include "../libs/server/accessinfo.h"
#include "../libs/server/socketmanager.h"
#include "../libs/socket/sendedmsg.h"

#include "tbmcommands.h"

#define MIN_USERID  100

class CTblMgrServer : public SocketManager, public AccessInfo
{
    public:

        static CTblMgrServer*		Instance();
        static void			FreeInst();
        static void			KillObject();
    private:

        CTblMgrServer();
        ~CTblMgrServer();

        /**
         * @brief Method for creating game table
         * @param _nUserID: uint32_t
         * @param _vecData: additional info about new table
         * @return void
         */
        void	Create( uint32_t _nUserID, const TVecChar* _vecData );

        /**
         * @brief Method for searching game tables with actual parameters
         * @param _nUserID: uint32_t
         * @param _vecData: parameters for searching
         * @return void
         */
        void    Find( uint32_t _nUserID, const TVecChar* _vecData );

	    /**
	     * @brief Method for request parameters of table
	     * @param _nUserID: uint32_t
	     * @param _nvecData: list of parameters IDs
	     * @return void
	    */
	    void GetParams( uint32_t _nUserID, const TVecChar *_vecData );

        void SetParams( uint32_t _nUserID, const TVecChar *_vecData );

        void DeleteTable( uint32_t _nUserID, uint32_t _nLogicID, uint32_t _nTableID );

        void GetMyTable( uint32_t _nUserID, uint32_t _nLogicID, uint32_t _nTableID );

        void    Random( uint32_t _nUserID, const TVecChar* _vecData );

        void	DoAllMsg( MySocket* );
        void	setSocket( MySocket * _pSocket );
        void	newMsg( ClientMsg* _pMsg );
        void	sendMsg( uint32_t _nTo, void* _pMsg, int _nSize );
        void    sendMsg( uint32_t _nTo, CSendedMsg* _pMsg );
    private:

        MySocket			*m_pSocket;
        TbmCommands			m_TbmCommands;
        static CTblMgrServer*		m_pSelf;
        static int			m_nRefCount;

};

#endif
