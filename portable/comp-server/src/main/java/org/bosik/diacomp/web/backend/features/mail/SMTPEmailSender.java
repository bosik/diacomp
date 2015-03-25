package org.bosik.diacomp.web.backend.features.mail;

import java.util.Properties;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

/**
 * Sends email via SMTP<br/>
 * Encoding: utf-8<br/>
 * Content Type: text/html
 */
public class SMTPEmailSender extends AbstractEmailSender
{
	private String	hostAddress;
	private String	hostUsername;
	private String	hostPassword;

	public SMTPEmailSender(String hostAddress, String hostUsername, String hostPassword)
	{
		this.hostAddress = hostAddress;
		this.hostUsername = hostUsername;
		this.hostPassword = hostPassword;
	}

	@Override
	public void send(String title, String body, InternetAddress sender, InternetAddress[] directReceivers,
			InternetAddress[] copyReceivers) throws MessagingException
	{
		Properties props = System.getProperties();
		props.put("mail.smtp.host", hostAddress);
		props.put("mail.smtp.auth", "true");
		props.put("mail.smtp.socketFactory.port", "465");
		props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");

		Session session = Session.getDefaultInstance(props, new javax.mail.Authenticator()
		{
			@Override
			protected PasswordAuthentication getPasswordAuthentication()
			{
				return new PasswordAuthentication(hostUsername, hostPassword);
			}
		});

		MimeMessage message = new MimeMessage(session);
		message.setSubject(title);
		message.setText(body);
		message.setHeader("Content-Type", "text/html;charset=utf-8");
		message.setFrom(sender);
		message.addRecipients(Message.RecipientType.TO, directReceivers);
		message.addRecipients(Message.RecipientType.CC, copyReceivers);

		Transport.send(message, message.getAllRecipients());
		// SMTPTransport transport = (SMTPTransport)
		// session.getTransport("smtp");
		// transport.setStartTLS(true);
		// transport.connect(hostAddress, hostUsername, hostPassword);
		// transport.sendMessage(message, message.getAllRecipients());
	}
}
