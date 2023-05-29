package br.com.sbk.sbking.gui.screens;

import static br.com.sbk.sbking.logging.SBKingLogger.LOGGER;

import br.com.sbk.sbking.core.Deal;
import br.com.sbk.sbking.gui.frames.SBKingClientJFrame;
import br.com.sbk.sbking.gui.main.ClientApplicationState;
import br.com.sbk.sbking.gui.painters.Painter;
import br.com.sbk.sbking.networking.client.SBKingClient;

public class MinibridgeScreen extends GameScreen {

  public MinibridgeScreen(SBKingClient sbkingClient) {
    super(sbkingClient);
  }

  @Override
  public void runAt(SBKingClientJFrame sbkingClientJFrame) {
    waitForDirection();

    while (true) {
      sleepFor(WAIT_FOR_REDRAW_IN_MILISECONDS);
      if (!this.checkIfStillIsOnGameScreen()) {
        return; // Exits when server says it is not on the game anymore.
      }
      if (sbkingClient.isSpectator()) {
        if (sbkingClient.getDealHasChanged() || ClientApplicationState.getGUIHasChanged()) {
          if (!ClientApplicationState.getGUIHasChanged()) {
            LOGGER.trace("Deal has changed. Painting deal.");
            LOGGER.trace("It is a spectator.");
          }
          Deal currentDeal = sbkingClient.getDeal();
          if (currentDeal != null) {
            Painter painter = this.painterFactory.getSpectatorPainter(currentDeal, sbkingClient.getActionListener());
            sbkingClientJFrame.paintPainter(painter);
          }
        }
      } else {
        if (sbkingClient.getDealHasChanged() || ClientApplicationState.getGUIHasChanged()) {
          if (!ClientApplicationState.getGUIHasChanged()) {
            LOGGER.trace("Deal has changed. Painting deal.");
            LOGGER.trace("It is a player.");
          }
          LOGGER.trace("Starting to paint Deal");
          Painter painter = this.painterFactory.getDealWithDummyPainter(sbkingClient.getDeal(),
              sbkingClient.getDirection(), sbkingClient.getActionListener());
          sbkingClientJFrame.paintPainter(painter);
          LOGGER.trace("Finished painting Deal");
        } else {
          if (!sbkingClient.isRulesetValidSet()) {
            LOGGER.debug("Suit not selected yet!");
            if (sbkingClient.getDirection() == null || !sbkingClient.isGameModeOrStrainChooserSet()) {
              LOGGER.trace("Direction not set yet.");
              LOGGER.trace("or Chooser not set yet.");
              continue;
            } else {
              LOGGER.trace("paintWaitingForChoosingGameModeOrStrainScreen!");
              LOGGER.trace("My direction: " + sbkingClient.getDirection());
              LOGGER.trace("Chooser: " + sbkingClient.getGameModeOrStrainChooser());

              Painter painter = this.painterFactory.getWaitingForChoosingGameModeOrStrainPainter(
                  sbkingClient.getDirection(), sbkingClient.getGameModeOrStrainChooser(), true);
              sbkingClientJFrame.paintPainter(painter);
              while (!sbkingClient.isRulesetValidSet()) {
                sleepFor(WAIT_FOR_SERVER_MESSAGE_IN_MILISECONDS);
              }
            }
          }
        }
      }
    }
  }

}
