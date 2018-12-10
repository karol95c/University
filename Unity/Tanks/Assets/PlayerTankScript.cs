using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerTankScript : TankScript {

	// Use this for initialization
	int lives = 3;
	void Start () {
		rigidBody = GetComponent<Rigidbody>();
		tankAudio = GetComponents<AudioSource>();
		ammo = 1000;
		ammoReloaded = true;
	}
	
	// Update is called once per frame
	void Update () {
		Move();
		
	}

	private void Move()
	{
		rigidBody.freezeRotation = false;
		if (Input.GetKeyUp(KeyCode.Space))
		{
			if(ammoReloaded) StartCoroutine(Wait(0.8f));
		}
		if (Input.GetKey(KeyCode.W))
		{
			if (!tankAudio[0].isPlaying)tankAudio[0].Play();
			rigidBody.AddRelativeForce(-Vector3.forward * mainThrust);
		}
		else if(Input.GetKey(KeyCode.S))
		{
			if (!tankAudio[0].isPlaying)tankAudio[0].Play();
			rigidBody.AddRelativeForce(Vector3.forward * mainThrust);
		}
		if (Input.GetKeyUp(KeyCode.W) || Input.GetKeyUp(KeyCode.S))
		{
			tankAudio[0].Stop();
		}

		float rotationThisFrame = Time.deltaTime * rcsThrust;
		if (Input.GetKey(KeyCode.A))
		{
			if (!tankAudio[1].isPlaying && !tankAudio[0].isPlaying) tankAudio[1].Play();
			transform.Rotate(-Vector3.up * rotationThisFrame);
		}
		else if (Input.GetKey(KeyCode.D))
		{
			if (!tankAudio[1].isPlaying && !tankAudio[0].isPlaying)tankAudio[1].Play();
			transform.Rotate(Vector3.up * rotationThisFrame);
		}
		if (Input.GetKeyUp(KeyCode.A) || Input.GetKeyUp(KeyCode.D))
		{
			tankAudio[1].Stop();
		}

		rigidBody.freezeRotation = false;
	}


	IEnumerator Wait(float duration)
    {
        //This is a coroutine
		ammoReloaded = false;
		Fire();
        yield return new WaitForSeconds(duration);   //Wait
		ammoReloaded = true;
    }

	public void gotHit()
	{
		--lives;
		if (lives == 0)
		{
			gameOver();
		}
	}

	private void gameOver()
	{
		//to implement;
	}
	
}
