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

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

@Entity
@Builder
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "user")
public class UserEntity
{
	@Id
	@Column(name = "id")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private int id;

	@Column(name = "login")
	private String name;

	@Column(name = "hash_pass", columnDefinition = "CHAR")
	private String hashPass;

	@Column(name = "activation_key", columnDefinition = "CHAR(64)")
	private String activationKey;

	@Column(name = "restore_key", columnDefinition = "CHAR(64)")
	private String restoreKey;

	@Column(name = "date_sign_up")
	private Date registrationDate;

	@Column(name = "date_sign_in")
	private Date loginDate;

	@Column(name = "date_deleted")
	private Date deletionDate;

	@Column(name = "login_deleted")
	private String loginDeleted;

	public boolean isDeleted()
	{
		return deletionDate != null && deletionDate.before(new Date());
	}
}
