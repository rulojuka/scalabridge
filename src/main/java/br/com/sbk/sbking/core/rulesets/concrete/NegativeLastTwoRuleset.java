package br.com.sbk.sbking.core.rulesets.concrete;

import br.com.sbk.sbking.core.Trick;
import br.com.sbk.sbking.core.rulesets.abstractClasses.NegativeRuleset;
import br.com.sbk.sbking.core.rulesets.implementations.DefaultSuitFollowable;
import br.com.sbk.sbking.core.rulesets.implementations.DontProhibitsHearts;
import br.com.sbk.sbking.core.rulesets.implementations.NoTrumpSuitWinnable;

@SuppressWarnings("serial")
public class NegativeLastTwoRuleset extends NegativeRuleset {

	private static final int NEGATIVE_LAST_TWO_SCORE_MULTIPLIER = 90;
	private static final int NUMBER_OF_LAST_TWO_TRICKS = 2;

	public NegativeLastTwoRuleset() {
		this.suitFollowable = new DefaultSuitFollowable();
		this.heartsProhibitable = new DontProhibitsHearts();
		this.winnable = new NoTrumpSuitWinnable();
	}

	@Override
	public int getScoreMultiplier() {
		return NEGATIVE_LAST_TWO_SCORE_MULTIPLIER;
	}

	@Override
	public int getPoints(Trick trick) {
		if (trick.isLastTwo()) {
			return 1;
		} else {
			return 0;
		}
	}

	@Override
	public String getShortDescription() {
		return "Negative last two";
	}

	@Override
	public String getCompleteDescription() {
		return "Avoid the last two tricks";
	}

	@Override
	public int getTotalPoints() {
		return NUMBER_OF_LAST_TWO_TRICKS;
	}

}
