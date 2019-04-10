package br.com.sbk.sbking.gui.frames;

import static br.com.sbk.sbking.gui.constants.FrameConstants.TABLE_COLOR;
import static br.com.sbk.sbking.gui.constants.FrameConstants.TABLE_HEIGHT;
import static br.com.sbk.sbking.gui.constants.FrameConstants.TABLE_WIDTH;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.swing.JFrame;

import org.apache.log4j.Logger;

import br.com.sbk.sbking.core.Direction;
import br.com.sbk.sbking.gui.models.GameScoreboard;
import br.com.sbk.sbking.gui.painters.ConnectToServerPainter;
import br.com.sbk.sbking.gui.painters.WaitingForChoosingGameModeOrStrainPainter;
import br.com.sbk.sbking.gui.painters.WaitingForChoosingPositiveOrNegativePainter;
import br.com.sbk.sbking.gui.painters.WaitingForPlayersPainter;
import br.com.sbk.sbking.networking.SBKingClient;

@SuppressWarnings("serial")
public class NetworkClientScreen extends JFrame {

	final static Logger logger = Logger.getLogger(NetworkClientScreen.class);

	private boolean connectedToServer = false;
	private SBKingClient sbKingClient;

	private ExecutorService pool;

	public NetworkClientScreen() {
		super();
		initializeJFrame();
		initializeContentPane();
		pool = Executors.newFixedThreadPool(4);
	}

	private void initializeJFrame() {
		this.setVisible(true);
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setSize(TABLE_WIDTH, TABLE_HEIGHT);
	}

	private void initializeContentPane() {
		getContentPane().setLayout(null);
		getContentPane().setBackground(TABLE_COLOR);
	}

	public void run() {
		logger.info("Starting to paint ConnectToServerScreen");
		paintConnectToServerScreen();
		logger.info("Finished painting ConnectToServerScreen");

		logger.info("Waiting for connectedToServer to be true");
		while (!connectedToServer) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		logger.info("Waiting for sbKingClient.isDirectionSet() to be true");
		while (!sbKingClient.isDirectionSet()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		logger.info("Starting to paint WaitingForPlayersScreen");
		paintWaitingForPlayersScreen();
		logger.info("Finished painting WaitingForPlayersScreen");

		logger.info("Waiting for sbKingClient.isEveryoneConnected() to be true");
		while (!sbKingClient.isEveryoneConnected()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		// while(true) { //FIXME Should last 10 deals - a complete king game
		
		logger.info("Waiting for sbKingClient.isPositiveOrNegativeChooserSet() to be true");
		while (!sbKingClient.isPositiveOrNegativeChooserSet()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		logger.info("Starting to paint WaitingForChoosingScreen");
		paintWaitingForChoosingPositiveOrNegativeScreen(sbKingClient.getDirection(), sbKingClient.getPositiveOrNegativeChooser());
		logger.info("Finished painting WaitingForChoosingScreen");

		logger.info("Waiting for sbKingClient.isPositiveOrNegativeSelected() to be true");
		while (!sbKingClient.isPositiveOrNegativeSelected()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		boolean isPositive = this.sbKingClient.isPositive();
		logger.info("Received PositiveOrNegative from server.");
		
		logger.info("Waiting for sbKingClient.isGameModeOrStrainChooserSet() to be true");
		while (!sbKingClient.isGameModeOrStrainChooserSet()) {
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		logger.info("Starting to paint WaitingForChoosingGameModeOrStrainScreen");
		paintWaitingForChoosingGameModeOrStrainScreen(sbKingClient.getDirection(), sbKingClient.getGameModeOrStrainChooser(),
				isPositive);
		logger.info("Finished painting WaitingForChoosingGameModeOrStrainScreen");

		// }

		logger.info("Finished!");
	}

	private void paintConnectToServerScreen() {
		cleanContentPane();
		ConnectToServerPainter connectToServerPainter = new ConnectToServerPainter(this);
		connectToServerPainter.paint(this.getContentPane());
	}

	private void paintWaitingForPlayersScreen() {
		cleanContentPane();
		WaitingForPlayersPainter waitingForPlayersPainter = new WaitingForPlayersPainter(sbKingClient.getDirection());
		waitingForPlayersPainter.paint(this.getContentPane());
	}

	private void paintWaitingForChoosingPositiveOrNegativeScreen(Direction myDirection, Direction chooserDirection) {
		cleanContentPane();
		WaitingForChoosingPositiveOrNegativePainter waitingForChoosingPainter = new WaitingForChoosingPositiveOrNegativePainter(myDirection,
				chooserDirection, this.sbKingClient);
		waitingForChoosingPainter.paint(this.getContentPane(), new GameScoreboard()); //FIXME should receive from server.
	}

	private void paintWaitingForChoosingGameModeOrStrainScreen(Direction direction, Direction chooser,
			boolean isPositive) {
		cleanContentPane();
		WaitingForChoosingGameModeOrStrainPainter waitingForChoosingGameModeOrStrainPainter = new WaitingForChoosingGameModeOrStrainPainter(
				direction, chooser, isPositive, this.sbKingClient);
		waitingForChoosingGameModeOrStrainPainter.paint(this.getContentPane(), new GameScoreboard());
	}

	private void cleanContentPane() {
		this.getContentPane().removeAll();
	}

	private void initializeWaitingForPlayerElement() {
		logger.info("Entrou initializeWaitingForPlayerElement");
//		this.setVisible(false);
//		this.dispose();
	}

	public void connectToServer() {
		this.sbKingClient = new SBKingClient();
		this.connectedToServer = true;
		pool.execute(this.sbKingClient);
	}

	class GameSelectActionListener implements java.awt.event.ActionListener {
		public void actionPerformed(java.awt.event.ActionEvent event) {
			SBKingClient sbKingClient = new SBKingClient();
			sbKingClient.run();
			initializeWaitingForPlayerElement();
		}

	}

}
