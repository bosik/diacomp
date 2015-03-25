package org.bosik.diacomp.web.backend.features.mail;

import javax.mail.MessagingException;
import javax.mail.internet.InternetAddress;

/**
 * Prints some debug info into console instead of actual email sending
 */
public class FakeEmailSender extends AbstractEmailSender
{
	private static final boolean	PRINT_DEBUG_INFO	= true;

	public FakeEmailSender(String hostAddress, String hostUsername, String hostPassword)
	{
		if (!PRINT_DEBUG_INFO) return;

		// to print complete log message at once.
		// a little bit overheaded
		StringBuilder sb = new StringBuilder();

		sb.append("Fake email sender created\n");
		sb.append("\tHost address: " + hostAddress + "\n");
		sb.append("\tHost username: " + hostUsername + "\n");
		sb.append("\tHost password: " + hostPassword);

		System.out.println(sb.toString());
	}

	@Override
	public void send(String title, String body, InternetAddress sender, InternetAddress[] directReceivers,
			InternetAddress[] copyReceivers) throws MessagingException
	{
		if (!PRINT_DEBUG_INFO) return;

		// to print complete log message at once
		StringBuilder sb = new StringBuilder();

		sb.append("Sending fake email\n");
		sb.append("\tTitle: " + title);
		//        sb.append("\n\tFrom: " + sender.toString() + "\n");
		//        sb.append("\tDirect receivers (" + directReceivers.length + "):\n");
		//        for (InternetAddress addr : directReceivers) {
		//            sb.append("\t\t" + addr.toString() + "\n");
		//        }
		//        sb.append("\tCopy receivers (" + copyReceivers.length + "):\n");
		//        for (InternetAddress addr : copyReceivers) {
		//            sb.append("\t\t" + addr.toString() + "\n");
		//        }
		sb.append("\n\tBody: " + body);

		System.out.println(sb.toString());

		// yes, concatenation isn't perfect here
	}
}
