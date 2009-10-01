#ifndef GAMMONLOGIC_H
#define GAMMONLOGIC_H

#include "../libs/header/interface.h"

#include "bglight/bgmatch.h"
#include "bglight/bgdefs.h"

#include <sstream>
#include <memory>
/**
	@author WapPortal.RU <office@wapportal.ru>
*/


class CGammonLogic : public IGameLogic
{

public:

    CGammonLogic();

    ~CGammonLogic();

    IGameLogic::StepRes StepAnl( TVByte* _pvecbtPos );

    bool SetPos( const TVByte& _vecbtPos );

    const TVByte*  GetPosForDB();

    const TVByte*  GetPosForClient();

    enum{ BAR_X = 24, BAR_O, DICE0, DICE1,  STEP, SIZE };

private:

    void StartNewGame();

    void InitDataForClient();

    void NextActions();

    bool CheckSide(int _side);

    void InitDice( TVByte* _pvecbtPos );

private:

    std::stringstream m_stream;

    TVByte m_vecPos;

    TVByte m_vecPosForClient;

    typedef std::auto_ptr<BG::Match > TAPMatch;

    TAPMatch m_pMatch;

    const BG::Match::Action* m_pAction;

};

#endif
