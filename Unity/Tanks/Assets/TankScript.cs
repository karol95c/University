using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TankScript : MonoBehaviour {

	// Use this for initialization
	protected float rcsThrust = 90f;
	protected float mainThrust = 250f;
	protected Rigidbody rigidBody;
	protected AudioSource[] tankAudio;
    public GameObject bulletPrefab;
	public Transform bulletSpawn;
	protected bool ammoReloaded;
	protected int ammo;
	
	void Start () {
		rigidBody = GetComponent<Rigidbody>();
		tankAudio = GetComponents<AudioSource>();
		ammo = 1000;
		ammoReloaded = true;
	}
	
	// Update is called once per frame
	void Update () {
	}

	protected void Fire()
	{ 
		if(ammo > 0)
		{
			tankAudio[2].Play();
			var bullet = (GameObject)Instantiate (
			bulletPrefab,
			bulletSpawn.position,
			bulletSpawn.rotation);
			// Add velocity to the bullet
			bullet.GetComponent<Rigidbody>().velocity = -bullet.transform.forward * 10;
			Destroy(bullet, 2.0f);
			// Destroy the bullet after 2 seconds
			--ammo;
		}
		else
		{
			if (!tankAudio[3].isPlaying)tankAudio[3].Play();
		} 
	}
}
