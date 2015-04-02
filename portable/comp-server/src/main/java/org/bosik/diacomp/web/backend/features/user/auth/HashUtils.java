/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.web.backend.features.user.auth;

import java.math.BigInteger;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;

/*
 * Password Hashing With PBKDF2 (http://crackstation.net/hashing-security.htm).
 * Copyright (c) 2013, Taylor Hornby
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
public class HashUtils
{
	private static final String	ALGORITHM			= "PBKDF2WithHmacSHA1";

	private static final int	ITERATIONS_COUNT	= 1024;
	private static final int	SALT_SIZE			= 16;					// bytes
	private static final int	HASH_SIZE			= 16;					// bytes

	public static String createHash(String password)
	{
		try
		{
			SecureRandom random = new SecureRandom();
			byte[] salt = new byte[SALT_SIZE];
			random.nextBytes(salt);

			byte[] hash = pbkdf2(password.toCharArray(), salt, ITERATIONS_COUNT, HASH_SIZE);
			return ITERATIONS_COUNT + ":" + writeHex(salt) + ":" + writeHex(hash);
		}
		catch (NoSuchAlgorithmException | InvalidKeySpecException e)
		{
			throw new RuntimeException(e);
		}
	}

	public static boolean validatePassword(String password, String correctHash)
	{
		try
		{
			String[] params = correctHash.split(":");
			int iterations = Integer.parseInt(params[0]);
			byte[] salt = readHex(params[1]);
			byte[] hash = readHex(params[2]);
			byte[] testHash = pbkdf2(password.toCharArray(), salt, iterations, hash.length);
			return slowEquals(hash, testHash);
		}
		catch (NoSuchAlgorithmException | InvalidKeySpecException e)
		{
			throw new RuntimeException(e);
		}
		catch (Exception e)
		{
			e.printStackTrace();
			return false;
		}
	}

	private static boolean slowEquals(byte[] a, byte[] b)
	{
		int diff = a.length ^ b.length;
		for (int i = 0; i < a.length && i < b.length; i++)
			diff |= a[i] ^ b[i];
		return diff == 0;
	}

	private static byte[] pbkdf2(char[] password, byte[] salt, int iterations, int bytes)
			throws NoSuchAlgorithmException, InvalidKeySpecException
	{
		PBEKeySpec spec = new PBEKeySpec(password, salt, iterations, bytes * 8);
		SecretKeyFactory skf = SecretKeyFactory.getInstance(ALGORITHM);
		return skf.generateSecret(spec).getEncoded();
	}

	private static byte[] readHex(String hex)
	{
		byte[] binary = new byte[hex.length() / 2];
		for (int i = 0; i < binary.length; i++)
		{
			binary[i] = (byte)Integer.parseInt(hex.substring(2 * i, 2 * i + 2), 16);
		}
		return binary;
	}

	private static String writeHex(byte[] array)
	{
		BigInteger bi = new BigInteger(1, array);
		String hex = bi.toString(16);
		int paddingLength = (array.length * 2) - hex.length();
		if (paddingLength > 0) return String.format("%0" + paddingLength + "d", 0) + hex;
		else return hex;
	}
}
